# Copyright 2024 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

{
  # DOOMDIR / Doom private directory / module.
  doomDir,
  # Default DOOMLOCALDIR (use something like ~/.local/share/doom).
  doomLocalDir,
  # Doom source tree.
  doomSource,
  # Emacs package to build against.
  emacs,
  # Name of doom profile to use. Empty string to disable profile early in startup.
  profileName ? "nix",
  # Use fetchTree instead of fetchGit for package fetches.
  experimentalFetchTree ? false,
  # Extra emacs packages from nixpkgs
  extraPackages ? epkgs: [ ],
  # Extra packages to add to $PATH
  extraBinPackages ? [
    git
    fd
    ripgrep
  ],
  # Args to pass to `doom +org tangle`.
  tangleArgs ? null,
  # true to build lsp-mode and dependant packages with LSP_USE_PLISTS set
  lspUsePlists ? true,

  callPackage,
  callPackages,
  fd,
  git,
  ripgrep,
  emacsPackagesFor,
  lib,
  runCommandLocal,
  runtimeShell,
  writeText,
  makeBinaryWrapper,
  stdenv,
  stdenvNoCC,
  toInit,
  writeTextDir,
}:
let
  inherit (import ./fetch-overrides.nix) extraFiles extraPins extraUrls;

  nonEmptyProfileName = if profileName != "" then profileName else "nix";

  tangleDoomDir = writeTextDir "init.el" (
    toInit lib {
      lang.org = true;
    }
  );

  # Preprocess DOOMDIR with `doom +org tangle` if requested.
  doomDir' =
    if tangleArgs != null then
      runCommandLocal "tangled-doomdir"
        {
          inherit
            tangleArgs
            doomDir
            doomSource
            runtimeShell
            ;
          EMACS = lib.getExe emacs;
          DOOMDIR = tangleDoomDir;
          # Enable this to troubleshoot failures at this step.
          #DEBUG = "1";
        }
        ''
          mkdir $out doomlocaldir
          export DOOMLOCALDIR=$PWD/doomlocaldir
          cd $out
          ln -s $doomDir/* ./
          $runtimeShell $doomSource/bin/doom +org tangle $tangleArgs
        ''
    else
      doomDir;

  # Step 1: determine which Emacs packages to pull in.
  #
  # Inputs: Doom, original DOOMDIR (only init.el and packages.el are used).
  # Outputs:
  # - Packages Doom normally loads using Straight (as json)
  # - modified packages.el that claims all packages are system-installed
  #
  # Uses Doom's CLI framework, which does not require anything else is installed
  # (not even straight).

  # Force local build in case the user init.el does something weird and to avoid a roundtrip.
  doomIntermediates = callPackage ./build-helpers/doomscript.nix {
    name = "doom-intermediates";
    inherit doomSource emacs;
    extraArgs = {
      DOOMDIR = "${doomDir'}";
    };
    script = ./build-helpers/dump;
    scriptArgs = "-o $out";
  };

  # Ignore agda-input: nixpkgs installs this as part of agda2-mode.
  doomPackageSet = lib.filterAttrs (n: v: n != "agda-input") (
    lib.importJSON "${doomIntermediates}/packages.json"
  );

  # Step 2: override Emacs packages to respect Doom's pins.
  doomEmacsPackages = (emacsPackagesFor emacs).overrideScope (
    eself: esuper:
    let
      customPackages = callPackages ./elisp-packages.nix { inherit emacs esuper eself; };
      # If multiple packages are built from the same repository, straight.el pins the repository
      # if only one of them is pinned. Doom relies on this behavior, so try to do the same.
      #
      # We need to do this for dependencies that are not in doomPackageSet. But we don't collect
      # all extra pins here, as that would involve pulling the repository from all packages in
      # esuper. Instead we map repositories to pins, and then do the rest of the work in
      # makePackage.

      # TODO: refactor url determination out of makePackage, use here?
      # Probably best done at the same time as the codeberg TODO in fetch-overrides.nix.
      repoToPin =
        let
          # Not unique, but that's ok as this is only used with genAttrs.
          packageNames = lib.attrNames doomPackageSet;
          packageToRepo = lib.genAttrs packageNames (name: esuper.${name}.src.gitRepoUrl or null);
          repoToPackages = lib.zipAttrs (lib.mapAttrsToList (name: repo: { ${repo} = name; }) packageToRepo);
          packageToPin = lib.mapAttrs (name: p: extraPins.${name} or p.pin or null) doomPackageSet;
          repoToPins = lib.mapAttrs (
            name: packages: lib.unique (lib.filter (p: p != null) (map (p: packageToPin.${p}) packages))
          ) repoToPackages;
        in
        lib.mapAttrs (
          name: pins:
          assert lib.assertMsg ((lib.length pins) <= 1) ''
            ${name}:
              used by ${lib.concatStringsSep ", " repoToPackages.${name}}
              pinned to different versions ${lib.concatStringsSep ", " pins}

            nix-doom-emacs-unstraightened assumes these packages would use the same repo
            when Doom Emacs builds them using straight.el, meaning this would not work.

            If that assumption is correct, this is a bug in Doom Emacs.

            If that assumption is not correct, this is a bug in Unstraightened.

            If unsure, report this as a bug in Unstraightened.'';
          lib.findFirst (lib.const true) null pins
        ) repoToPins;

      # We want to override `version` along with `src` to avoid spurious
      # rebuilds on version bumps in emacs-overlay of packages Doom has
      # pinned.
      #
      # The elisp manual says we need a version `version-to-list` can parse,
      # which means it must start with a number and cannot contain the actual
      # commit ID. We start with a large integer in case package.el starts
      # version-checking dependencies (it currently does not but a comment in
      # the code says it should). Additionally, `(package-version-join
      # (version-to-list v))` must roundtrip to avoid elpa2nix failing with
      # "Package does not untar cleanly".
      #
      # Additionally, we currently need this version to be recognized by
      # https://github.com/NixOS/nixpkgs/blob/26b2bef8b3c73a0931af73d902af2d806588f6bb/pkgs/applications/editors/emacs/build-support/elpa2nix.el#L4-L12
      # That means we need a digit after "snapshot".
      snapshotVersion = "9999snapshot1";

      makePackage =
        name: p:
        assert lib.asserts.assertEachOneOf "keys for ${name}" (lib.attrNames p) [
          "modules"
          "recipe"
          "pin"
          "type"
          "env" # ignored. Used by doom for LSP_USE_PLISTS: revisit if its use spreads.
        ];
        assert (p ? type) -> lib.asserts.assertOneOf "type of ${name}" p.type [ "core" ];
        let
          # We're called for all attributes of esuper (to check if they're a package pinned via
          # repoToPin). Some of those attributes are null. So we cannot use `esuper.${name} or
          # null`, we need to explicitly check for presence.
          hasOrigEPkg = esuper ? ${name};
          origEPkg = esuper.${name};
          pin =
            extraPins.${name} or p.pin or (
              # Don't use `url`: this needs to be in sync with repoToPin above.
              # (If we remap ELPA packages to emacs-straight here but not above, it breaks...)
              let
                repo = esuper.${name}.src.gitRepoUrl or null;
              in
              if repo != null then repoToPin.${repo} or null else null
            );
          files = p.recipe.files or extraFiles.${name} or null;
          # We have to specialcase ELPA packages pinned by Doom: Straight mirrors /
          # repackages them. Doom's pins assume that mirror is used (so we have to
          # use it), and replacing the source in nixpkgs's derivation will not work
          # (it assumes it gets a tarball as input).

          # TODO: check notmuch works correctly without notmuch-version.el

          isElpa =
            hasOrigEPkg
            && (
              origEPkg == esuper.elpaPackages.${name} or null || origEPkg == esuper.nongnuPackages.${name} or null
            );
          epkg =
            if hasOrigEPkg && (pin != null -> !(isElpa || customPackages ? ${name})) then
              origEPkg
            else
              customPackages.${name} or (
                assert lib.assertMsg (isElpa || (p ? recipe) || extraUrls ? ${name}) ''
                  nix-doom-emacs-unstraightened: ${name}: unable to derive repository URL:
                  - no recipe provided
                  - not an ELPA package
                  - not in Unstraightened's fetch-overrides.nix

                  If this is a custom `package!` entry in packages.el, add a `:recipe`.
                '';
                # Assume we can safely ignore (pre-)build unless we're actually
                # building our own package.
                assert lib.assertMsg (!(p ? recipe.pre-build)) "${name}: pre-build not supported";
                assert lib.assertMsg (!(p ? recipe.build)) "${name}: build not supported";
                assert lib.assertMsg (pin != null) ''
                  nix-doom-emacs-unstraightened: ${name}: not in nixpkgs or emacs-overlay, not pinned.
                  All packages must be pinned so they can be fetched deterministically.

                  If this is a custom `package!` entry in your packages.el, add a `:pin`.
                  If it is a `package!` in Doom, add an entry to Unstraightened's fetch-overrides.nix.
                '';

                # This uses melpaBuild instead of trivialBuild to end up with
                # something package.el understands as satisfying dependencies.
                # This is necessary if we're replacing a pinned ELPA dependency
                # of an unpinned ELPA package.
                (esuper.melpaBuild {
                  pname = name;
                  # melpaBuild requires we set `version` and `commit` here
                  # (leaving `version` unset until overrideAttrs below does not
                  # work).
                  version = snapshotVersion;
                  meta = {
                    description = "trivial build for doom-emacs";
                  };
                  inherit files;
                  # TODO: refactor out the recursive call to makePackage.
                  # (Currently needed for dependencies on packages not in epkgs or doom.)
                  packageRequires = map (name: eself.${name} or (makePackage name { })) reqlist;
                }).overrideAttrs
                  (prev: {
                    # We only depend on this during evaluation. Force a dependency so it does not
                    # get garbage-collected, which slows down subsequent evaluation.
                    inherit reqfile;
                    postInstall = (prev.postInstall or "") + ''
                      mkdir -p $out/nix-support
                      ln -s $reqfile $out/nix-support/unstraightened-dependencies.json
                    '';
                  })
              );
          # nixpkgs uses fetchZip for these, so epkg.src.gitRepoUrl is unset.
          # Derive the repo URL from the archive name, which will look like
          # https://codeberg.org/rwv/android-mode/archive/67f7c0d7d37605efc7f055b76d731556861c3eb9.tar.gz
          codeberg = lib.strings.match "(https://codeberg.org/[^/]+/[^/]+)/.*" (epkg.src.url or "");
          url =
            if (p.recipe.host or "") == "github" && p ? recipe.repo then
              "https://github.com/${p.recipe.repo}"
            else if (p.recipe.type or "git") == "git" && p ? recipe.repo && (p.recipe.host or null) == null then
              p.recipe.repo
            else
              extraUrls.${name} or epkg.src.gitRepoUrl or (
                if isElpa then
                  "https://github.com/emacs-straight/${name}"
                else if codeberg != null then
                  (lib.head codeberg) + ".git"
                else
                  (
                    let
                      recipe = lib.generators.toPretty { } (p.recipe or "missing");
                    in
                    throw "${name}: cannot derive url from recipe ${recipe}"
                  )
              );
          # Use the fetchGit primop instead of nixpkgs's fetchFromGitHub because
          # fetchGit allows fetching a specific git commit without a hash.
          fetchGitArgs = {
            inherit url;
            rev = pin;
            allRefs = true;
            # Skip submodules by default because they seem to be hitting
            # https://github.com/NixOS/nix/issues/10773 (or a similar caching issue) and for
            # parity between fetchTree's github fetcher and fetchGit (GitHub's exports don't
            # seem to contain submodules).
            submodules = !(p.recipe.nonrecursive or true);
            # TODO: pull ref from derivation.src when not pulling it from p.recipe?
            # Note Doom does have packages with pin + branch (or nonrecursive) set,
            # expecting to inherit the rest of the recipe from Straight.

            # Always specify a ref to work around https://github.com/NixOS/nix/issues/10773
            ref = p.recipe.branch or "HEAD";

            # TODO: remove if https://github.com/NixOS/nix/issues/11012 is fixed.
            shallow = false;
          };
          src =
            if experimentalFetchTree then
              fetchTree (
                if lib.hasPrefix "https://github.com/" url then
                  let
                    tail = lib.removePrefix "https://github.com/" url;
                    split = lib.splitString "/" tail;
                    owner = lib.head split;
                    repo = lib.removeSuffix ".git" (lib.elemAt split 1);
                  in
                  {
                    type = "github";
                    inherit owner repo;
                    rev = pin;
                  }
                else if lib.hasPrefix "https://git.sr.ht/" url then
                  let
                    tail = lib.removePrefix "https://git.sr.ht/" url;
                    split = lib.splitString "/" tail;
                    owner = lib.head split;
                    repo = lib.elemAt split 1;
                  in
                  {
                    type = "sourcehut";
                    inherit owner repo;
                    rev = pin;
                  }
                else
                  (
                    {
                      type = "git";
                    }
                    // fetchGitArgs
                  )
              )
            else
              fetchGit fetchGitArgs;
          # Run locally to avoid a network roundtrip.
          reqfile = runCommandLocal "${name}-deps" {
            inherit src name;
            emacs = lib.getExe emacs;
            printDeps = ./build-helpers/print-deps.el;
          } "$emacs -Q --batch --script $printDeps $src $name > $out";
          reqjson = lib.importJSON reqfile;
          # json-encode encodes the empty list as null (nil), not [].
          reqlist = if reqjson == null then [ ] else reqjson;
        in
        if pin != null then
          epkg.overrideAttrs {
            inherit src;
            version = snapshotVersion;
            commit = pin;
          }
        else
          epkg;
      # Hack: we call makePackage for everything (not just doomPackageSet), just to hit the
      # repoToPin check. We cannot easily call it just for transitive dependencies, because we
      # need makePackage to figure out what the dependencies (for packages not in esuper) are.
      # But we do need some filtering (currently just "emacs" itself) to avoid infinite recursion
      # while populating repoToPin.
      upstreamWithPins = lib.mapAttrs (
        n: p: if (!lib.isDerivation p) || lib.elem p [ esuper.emacs ] then p else makePackage n { }
      ) esuper;
      doomPackages = lib.mapAttrs makePackage doomPackageSet;
      allPackages = upstreamWithPins // doomPackages;
    in
    allPackages
  );
  doomEmacsPackages' = doomEmacsPackages.overrideScope (
    eself: esuper:
    let
      manglePackage =
        name: pkg:
        let
          # TODO: actually make the "or dependant" bit work without infinite recursion
          isLspModeOrDependant =
            # This causes infinite recursion, and I have not wrapped my head around why yet.
            # (name == "lsp-mode") || lib.elem esuper.lsp-mode pkg.packageRequires;
            (name == "lsp-mode")
            || (lib.elem "lsp-mode" (map (p: p.ename or "not-a-package") (pkg.packageRequires or [ ])));
        in
        pkg.overrideAttrs (
          lib.optionalAttrs isLspModeOrDependant {
            preBuild = (pkg.preBuild or "") + ''
              export LSP_USE_PLISTS=1
            '';
          }
        );
      maybeManglePackage =
        name: pkg: if (lib.isDerivation pkg) && pkg != esuper.emacs then (manglePackage name pkg) else pkg;
      result = lib.mapAttrs maybeManglePackage esuper;
    in
    result
  );

  # Step 3: Build an emacsWithPackages, pulling all packages from step 1 from
  # the set from step 2.
  emacsWithPackages = doomEmacsPackages'.emacsWithPackages (
    epkgs:
    (map (p: epkgs.${p}) (lib.attrNames doomPackageSet)) ++ (extraPackages epkgs) ++ extraBinPackages
  );

  # Step 4: build a DOOMDIR, Doom profile and profile loader using Emacs from
  # step 3 and packages.el from step 1.
  #
  # Do this all in one derivation because these refer back to each other:
  # - init.el in DOOMDIR refers to the straight.el build cache generated along
  #   with the profile
  # - The path to the generated profile is included in the loader
  # - Generating the profile depends on the loader

  doomProfile = stdenvNoCC.mkDerivation {
    name = "doom-profile";
    buildCommandPath = ./build-helpers/build-doom-profile.sh;

    inherit
      doomIntermediates
      doomSource
      runtimeShell
      ;
    doomDir = doomDir';
    profileName = nonEmptyProfileName;
    noProfileHack = profileName == "";
    buildProfileLoader = ./build-helpers/build-profile-loader;
    buildProfile = ./build-helpers/build-profile;
    initEl = ./init.el;
    EMACS = lib.getExe emacsWithPackages;
    inherit (emacsWithPackages) deps;
    # Enable this to troubleshoot failures at this step.
    #DEBUG = "1";

    # Required to avoid Doom erroring out at startup.
    nativeBuildInputs = [ git ];
    # Force local build in case the user init.el does something weird.
    preferLocalBuild = true;
    allowSubstitutes = false;
  };

  # Step 5: write wrappers to start the whole thing.

  # makeBinaryWrapper pulls in a compiler, so don't force this one local.
  doomEmacs = stdenv.mkDerivation {
    name = "doom-emacs";
    buildCommandPath = ./build-helpers/build-doom-emacs.sh;

    # emacsWithPackages also accessed externally (for pushing to Cachix).
    inherit
      doomProfile
      doomLocalDir
      doomSource
      emacsWithPackages
      lspUsePlists
      ;
    profileName = nonEmptyProfileName;

    nativeBuildInputs = [ makeBinaryWrapper ];
  };

  emacsWithDoom = stdenvNoCC.mkDerivation {
    inherit (lib.appendToName "with-doom" emacs) name;
    inherit (emacs) meta;
    inherit doomEmacs emacs;
    buildCommandPath = ./build-helpers/build-emacs-with-doom.sh;

    # Force local build as it's near-trivial.
    preferLocalBuild = true;
    allowSubstitutes = false;
  };
in
{
  inherit doomEmacs emacsWithDoom;
}

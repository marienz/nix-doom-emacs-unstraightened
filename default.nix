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
  /* DOOMDIR / Doom private directory / module. */
  doomDir,
  /* Default DOOMLOCALDIR.
   *
   * Required, because the default is relative to Doom's source tree,
   * which is read-only.
   *
   * Expanded using expand-file-name (an initial ~ is supported,
   * shell variable expansion is not).
   *
   * DOOMLOCALDIR in the environment Emacs is started with overrides this.
   *
   * Suggested value: ~/.local/share/doom
   */
  doomLocalDir,
  /* Doom source tree. */
  doomSource,
  /* Emacs package to build against. */
  emacs,
  /* Whether to enable all default dependencies. Primarily useful for CI /
     testing. */
  full ? false,
  /* Name of doom profile to use. */
  profileName ? "nix",
  /* Disable profile early in startup, so "normal" cache/state dirs are used. */
  noProfileHack ? false,

  callPackages,
  git,
  emacsPackagesFor,
  lib,
  runCommand,
  runCommandLocal,
  runtimeShell,
  writeText,
  makeBinaryWrapper,
}:
let
  inherit (lib) optionalAttrs optionalString;
  inherit (import ./fetch-overrides.nix) extraPins extraUrls allRefsRepos;

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
  doomIntermediates = runCommandLocal "doom-intermediates"
    {
      env = {
        EMACS = lib.getExe emacs;
        DOOMDIR = "${doomDir}";
        # Enable this to troubleshoot failures at this step.
        #DEBUG = "1";
      };
      # We set DOOMLOCALDIR somewhere harmless below to stop Doom from trying to
      # create it somewhere read-only.
    } ''
    mkdir $out
    export DOOMLOCALDIR=$(mktemp -d)
    ${runtimeShell} ${doomSource}/bin/doomscript ${./build-helpers/dump} \
      ${optionalString full "--full"} -o $out
  '';

  doomPackageSet = lib.importJSON "${doomIntermediates}/packages.json";

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
        repoToPin = let
          # Not unique, but that's ok as this is only used with genAttrs.
          packageNames = lib.attrNames doomPackageSet;
          packageToRepo = lib.genAttrs packageNames (name: esuper.${name}.src.gitRepoUrl or null);
          repoToPackages = lib.zipAttrs
            (lib.mapAttrsToList (name: repo: { ${repo} = name; }) packageToRepo);
          packageToPin = lib.mapAttrs
            (name: p: p.pin or extraPins.${name} or null) doomPackageSet;
          repoToPins = lib.mapAttrs (name: packages:
            lib.unique (lib.filter (p: p != null) (map (p: packageToPin.${p}) packages)))
            repoToPackages;
          in
            lib.mapAttrs (name: pins:
              assert lib.assertMsg ((lib.length pins) <= 1) ''
                ${name}:
                  used by ${lib.concatStringsSep ", " repoToPackages.${name}}
                  pinned to different versions ${lib.concatStringsSep ", " pins}

                nix-doom-emacs-unstraightened assumes these packages would use the same repo
                when Doom Emacs builds them using straight.el, meaning this would not work.

                If that assumption is correct, this is a bug in Doom Emacs.

                If that assumption is not correct, this is a bug in Unstraightened.

                If unsure, report this as a bug in Unstraightened.'';
              lib.findFirst (lib.const true) null pins)
              repoToPins;

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
        snapshotVersion = "9999snapshot";

        makePackage = name: p:
          assert lib.asserts.assertEachOneOf
            "keys for ${name}"
            (lib.attrNames p)
            [ "modules" "recipe" "pin" "type" ];
          assert (p ? type) -> lib.asserts.assertOneOf
            "type of ${name}"
            p.type
            [ "core" ];
          let
            # We're called for all attributes of esuper (to check if they're a package pinned via
            # repoToPin). Some of those attributes are null. So we cannot use `esuper.${name} or
            # null`, we need to explicitly check for presence.
            hasOrigEPkg = esuper ? ${name};
            origEPkg = esuper.${name};
            pin = p.pin or extraPins.${name} or (
              # Don't use `url`: this needs to be in sync with repoToPin above.
              # (If we remap ELPA packages to emacs-straight here but not above, it breaks...)
              let repo = esuper.${name}.src.gitRepoUrl or null; in
              if repo != null
              then repoToPin.${repo} or null
              else null);
            # We have to specialcase ELPA packages pinned by Doom: Straight mirrors /
            # repackages them. Doom's pins assume that mirror is used (so we have to
            # use it), and replacing the source in nixpkgs's derivation will not work
            # (it assumes it gets a tarball as input).

            # TODO: check notmuch works correctly without notmuch-version.el

            isElpa = hasOrigEPkg && (
              origEPkg == esuper.elpaPackages.${name} or null
              || origEPkg == esuper.nongnuPackages.${name} or null);
            epkg =
              customPackages.${name}
                or (if !hasOrigEPkg || (p ? pin && isElpa)
              then
                assert lib.assertMsg
                  (isElpa || (p ? recipe && pin != null) || extraUrls ? ${name})
                  "${name}: not in epkgs, not elpa, no recipe or not pinned";
                # Assume we can safely ignore (pre-)build unless we're actually
                # building our own package.
                assert lib.assertMsg (!(p ? recipe.pre-build)) "${name}: pre-build not supported";
                assert lib.assertMsg (!(p ? recipe.build)) "${name}: build not supported";
                # TODO: lift "pin" requirement, if that turns out to be
                # necessary or at least desirable. Requires figuring out why
                # melpa2nix requires `commit`. Not a priority because if it's
                # not in epkgs we'd need a recipe passed in, and it's uncommon
                # for Doom to pass in a recipe without pinning.
                #
                # Doom does currently have unpinned packages with an explicit
                # recipe, but they're in epkgs (popon and flymake-popon) so it
                # should be ok. Users might do this to pull in a custom package
                # they don't care about pinning, though: we may want to support
                # that.
                assert lib.assertMsg (pin != null)
                  "${name}: not in epkgs and not pinned. This is not yet supported.";
                # epkgs.*Build helpers take an attrset, they do not support
                # mkDerivation's fixed-point evaluation (`finalAttrs`).
                # If they did, the buildInputs stuff should use finalAttrs.src.

                # This uses melpaBuild instead of trivialBuild to end up with
                # something package.el understands as satisfying dependencies.
                # This is necessary if we're replacing a pinned ELPA dependency
                # of an unpinned ELPA package.
                esuper.melpaBuild {
                  pname = name;
                  # melpaBuild requires we set `version` and `commit` here
                  # (leaving `version` unset until overrideAttrs below does not
                  # work).
                  version = snapshotVersion;
                  commit = pin;
                  meta = {
                    description = "trivial build for doom-emacs";
                  };
                  # Just enough to make melpa2nix work.
                  recipe = writeText "${name}-generated-recipe" ''
                    (${name} :fetcher github :repo "marienz/made-up"
                     ${optionalString (p ? recipe.files) ":files ${p.recipe.files}"})'';
                  # TODO: refactor out the recursive call to makePackage.
                  # (Currently needed for dependencies on packages not in epkgs or doom.)
                  packageRequires = map (name: eself.${name} or (makePackage name {})) reqlist;
                }
                else origEPkg);
            url =
              if (p.recipe.host or "") == "github" && p ? recipe.repo
              then "https://github.com/${p.recipe.repo}"
              else epkg.src.gitRepoUrl
                or extraUrls.${name}
                or (if isElpa then "https://github.com/emacs-straight/${name}"
                  else (throw "${name}: cannot derive url from recipe ${p.recipe or "<missing>"}"));
            # Use builtins.fetchGit instead of nixpkgs's fetchFromGitHub because
            # fetchGit allows fetching a specific git commit without a hash.
            # TODO: port to fetchTree once (mostly) stable
            # (in particular the github fetcher may be noticably more efficient)
            src = builtins.fetchGit (
              {
                inherit url;
                rev = pin;
                allRefs = allRefsRepos.${url} or false;
                submodules = !(p.recipe.nonrecursive or false);
                # TODO: pull ref from derivation.src when not pulling it from p.recipe?
                # Note Doom does have packages with pin + branch (or nonrecursive) set,
                # expecting to inherit the rest of the recipe from Straight.
              } // optionalAttrs (p ? recipe.branch) { ref = p.recipe.branch; }
              // optionalAttrs (p ? recipe.depth) { shallow = p.recipe.depth == 1; }
            );
            # Run locally to avoid a network roundtrip.
            reqfile = runCommandLocal "${name}-deps" { } ''
              ${lib.getExe emacs} -Q --batch --script \
                ${./build-helpers/print-deps.el} ${src} > $out
            '';
            reqjson = lib.importJSON reqfile;
            # json-encode encodes the empty list as null (nil), not [].
            reqlist = if reqjson == null then [ ] else reqjson;
          in
          if pin != null
          then epkg.overrideAttrs {
            inherit src;
            version = snapshotVersion;
          }
          else epkg;
      in
        # Hack: we call makePackage for everything (not just doomPackageSet), just to hit the
        # repoToPin check. We cannot easily call it just for transitive dependencies, because we
        # need makePackage to figure out what the dependencies (for packages not in esuper) are...
        # This seems to work ok in practice because makePackage is called lazily.
        lib.mapAttrs makePackage ((lib.mapAttrs (name: (lib.const {})) esuper) // doomPackageSet)
  );

  # Step 3: Build an emacsWithPackages, pulling all packages from step 1 from
  # the set from step 2.
  emacsWithPackages = doomEmacsPackages.emacsWithPackages
    (epkgs: (map (p: epkgs.${p}) (builtins.attrNames doomPackageSet)));

  # Step 4: build a DOOMDIR, Doom profile and profile loader using Emacs from
  # step 3 and packages.el from step 1.
  #
  # Do this all in one derivation because these refer back to each other:
  # - init.el in DOOMDIR refers to the straight.el build cache generated along
  #   with the profile
  # - The path to the generated profile is included in the loader
  # - Generating the profile depends on the loader

  # Force local build in case the user init.el does something weird.
  doomProfile = runCommandLocal "doom-profile"
    {
      env = {
        EMACS = lib.getExe emacsWithPackages;
        # Enable this to troubleshoot failures at this step.
        #DEBUG = "1";
      };
      # Required to avoid Doom erroring out at startup.
      nativeBuildInputs = [ git ];
    } ''
    mkdir $out $out/loader $out/doomdir $out/profile $out/straight
    ln -s ${doomDir}/* $out/doomdir/
    # yasnippet logs an error at startup if snippets/ does not exist.
    if ! [[ -e $out/doomdir/snippets ]]; then
      mkdir $out/doomdir/snippets
    fi
    rm $out/doomdir/init.el
    if [[ -z "${profileName}" ]]; then
      setProfile="(setq doom-profile-dir \"$out/profile\")"
    else
      setProfile=""
    fi
    substitute ${./init.el} $out/doomdir/init.el \
      --subst-var-by maybe-set-profile-dir "$setProfile" \
      --subst-var-by profile-name "${profileName}" \
      --subst-var-by user-init "${doomDir}/init.el" \
      --subst-var-by straight-base-dir $out
    ln -sf ${doomIntermediates}/packages.el $out/doomdir/
    export DOOMDIR=$out/doomdir

    # DOOMLOCALDIR must be writable, Doom creates some subdirectories.
    export DOOMLOCALDIR=$(mktemp -d)
    if [[ -n "${profileName}" ]]; then
      export DOOMPROFILELOADFILE=$out/loader/init.el
      ${runtimeShell} ${doomSource}/bin/doomscript ${./build-helpers/build-profile-loader} \
        -n "${profileName}" -b "$out" ${optionalString noProfileHack "-u"}

      # With DOOMPROFILE set, doom-state-dir and friends are HOME-relative.
      export HOME=$(mktemp -d)
      export DOOMPROFILE='${profileName}';
    fi
    ${runtimeShell} ${doomSource}/bin/doomscript ${./build-helpers/build-profile}

    # Similar to audit-tmpdir.sh in nixpkgs.
    if grep -q -F "$TMPDIR/" -r $out; then
      echo "Doom profile contains a forbidden reference to $TMPDIR/"
      exit 1
    fi
  '';

  # Step 6: write wrappers to start the whole thing.

  # Use runCommand, not runCommandLocal, because makeBinaryWrapper pulls in a compiler.
  doomEmacs = runCommand "doom-emacs" {
    nativeBuildInputs = [ makeBinaryWrapper ];
  } ''
  if [[ -z "${profileName}" ]]; then
    profileArgs=()
  else
    profileArgs=(
      --set DOOMPROFILELOADFILE ${doomProfile}/loader/init.el
      --set DOOMPROFILE ${profileName}
    )
  fi
  makeWrapper ${emacsWithPackages}/bin/emacs $out/bin/doom-emacs \
    "''${profileArgs[@]}" \
    --set DOOMDIR ${doomProfile}/doomdir \
    --set-default DOOMLOCALDIR "${doomLocalDir}" \
    --add-flags "--init-directory=${doomSource}"
  makeWrapper ${doomSource}/bin/doomscript $out/bin/doomscript \
    --set EMACS ${emacsWithPackages}/bin/emacs \
    --set-default DOOMLOCALDIR "${doomLocalDir}"
  makeWrapper ${doomSource}/bin/doom $out/bin/doom \
    --set EMACS ${emacsWithPackages}/bin/emacs \
    "''${profileArgs[@]}" \
    --set DOOMDIR ${doomProfile}/doomdir \
    --set-default DOOMLOCALDIR "${doomLocalDir}"
  '';

  emacsWithDoom = runCommand (lib.appendToName "with-doom" emacs).name {
    inherit (emacs) meta;
  } ''
    mkdir -p $out/bin
    ln -s ${emacs}/bin/* $out/bin/
    rm $out/bin/emacs-*
    ln -sf ${doomEmacs}/bin/doom-emacs $out/bin/emacs
    ln -sf ${doomEmacs}/bin/{doom,doomscript} $out/bin/

    mkdir -p $out/share
    # Don't link everything: the systemd units would still refer to normal Emacs.
    # This links the same stuff emacsWithPackages does.
    for dir in applications icons info man; do
      ln -s ${emacs}/share/$dir $out/share/$dir
    done
  '';
in
{
  inherit doomEmacs emacsWithDoom;
}

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
  runtimeShell,
  writeText,
  makeBinaryWrapper,
}:
let
  inherit (lib) optionalAttrs optionalString;
  inherit (import ./fetch-overrides.nix) extraPins extraUrls;

  # Step 1: determine which Emacs packages to pull in.
  #
  # Inputs: Doom, original DOOMDIR (only init.el and packages.el are used).
  # Outputs:
  # - Packages Doom normally loads using Straight (as json)
  # - modified packages.el that claims all packages are system-installed
  #
  # Uses Doom's CLI framework, which does not require anything else is installed
  # (not even straight).

  # XXX this may need to be runCommandLocal just in case conditionals an init.el
  # / packages.el evaluate differently on build systems.
  doomIntermediates = runCommand "doom-intermediates"
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
            origEPkg = esuper.${name} or null;
            pin = p.pin or extraPins.${name} or null;
            # We have to specialcase ELPA packages pinned by Doom: Straight mirrors /
            # repackages them. Doom's pins assume that mirror is used (so we have to
            # use it), and replacing the source in nixpkgs's derivation will not work
            # (it assumes it gets a tarball as input).

            # TODO: check notmuch works correctly without notmuch-version.el

            isElpa = origEPkg != null && (origEPkg == esuper.elpaPackages.${name} or null || origEPkg == esuper.nongnuPackages.${name} or null);
            epkg =
              customPackages.${name}
                or (if origEPkg == null || (p ? pin && isElpa)
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
                  packageRequires = (map (name: eself.${name}) reqlist);
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
                submodules = !(p.recipe.nonrecursive or false);
                # TODO: might need to pull ref from derivation.src if we're not pulling it from p.recipe?
                # Note Doom does have packages with pin + branch (or nonrecursive) set,
                # expecting to inherit the rest of the recipe from Straight.
              } // optionalAttrs (p ? recipe.branch) { ref = p.recipe.branch; }
              // optionalAttrs (p ? recipe.depth) { shallow = p.recipe.depth == 1; }
            );
            reqfile = runCommand "${name}-deps" { } ''
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
      lib.mapAttrs makePackage doomPackageSet
  );

  # Step 3: Build an emacsWithPackages, pulling all packages from step 1 from
  # the set from step 2.
  emacsWithPackages = doomEmacsPackages.emacsWithPackages (epkgs: (map (p: epkgs.${p}) (builtins.attrNames doomPackageSet)));

  # Step 4: build a DOOMDIR, Doom profile and profile loader using Emacs from
  # step 3 and packages.el from step 1.
  #
  # Do this all in one derivation because these refer back to each other:
  # - init.el in DOOMDIR refers to the straight.el build cache generated along
  #   with the profile
  # - The path to the generated profile is included in the loader
  # - Generating the profile depends on the loader

  # XXX runCommandLocal? (See doomIntermediates.)
  doomProfile = runCommand "doom-profile"
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
    substitute ${./init.el} $out/doomdir/init.el \
      --subst-var-by user-init "${doomDir}/init.el" \
      --subst-var-by straight-base-dir $out
    ln -sf ${doomIntermediates}/packages.el $out/doomdir/
    export DOOMDIR=$out/doomdir

    export DOOMPROFILELOADFILE=$out/loader/init.el
    # DOOMLOCALDIR must be writable, Doom creates some subdirectories.
    export DOOMLOCALDIR=$(mktemp -d)
    ${runtimeShell} ${doomSource}/bin/doomscript ${./build-helpers/build-profile-loader} \
      -n "${profileName}" -b "$out" ${optionalString noProfileHack "-u"}

    # With DOOMPROFILE set, doom-state-dir and friends are HOME-relative.
    export HOME=$(mktemp -d)
    export DOOMPROFILE='${profileName}';
    ${runtimeShell} ${doomSource}/bin/doomscript ${./build-helpers/build-profile}

    # Similar to audit-tmpdir.sh in nixpkgs.
    if grep -q -F "$TMPDIR/" -r $out; then
      echo "Doom profile contains a forbidden reference to $TMPDIR/"
      exit 1
    fi
  '';

  # Step 6: write wrappers to start the whole thing.
  doomEmacs = runCommand "doom-emacs" {
    nativeBuildInputs = [ makeBinaryWrapper ];
  } ''
  makeWrapper ${emacsWithPackages}/bin/emacs $out/bin/doom-emacs \
    --set DOOMPROFILELOADFILE ${doomProfile}/loader/init.el \
    --set DOOMPROFILE ${profileName} \
    --set DOOMDIR ${doomProfile}/doomdir \
    --set-default DOOMLOCALDIR "${doomLocalDir}" \
    --add-flags "--init-directory=${doomSource}"
  makeWrapper ${doomSource}/bin/doomscript $out/bin/doomscript \
    --set EMACS ${emacsWithPackages}/bin/emacs \
    --set DOOMPROFILELOADFILE ${doomProfile}/loader/init.el \
    --set DOOMPROFILE ${profileName} \
    --set DOOMDIR ${doomProfile}/doomdir \
    --set-default DOOMLOCALDIR "${doomLocalDir}" \
  '';
  # TODO: revisit wrapping `doom` if/when profile use is optional.
  #
  # I would like to support `doom doctor` and user commands (from their
  # `cli.el)`.
  #
  # Wrapping it the same way I wrap emacs, passing EMACS in addition to
  # DOOMPROFILELOADFILE, DOOMPROFILE and DOOMLOCALDIR, almost works.
  # But with doomProfile set, `doom doctor` currently fails with
  #
  # Profile init file hasn’t been generated. Did you forgot to run ’doom sync’?
  #
  # It looks like this breaks because doom-start wants to load the profile via
  # (doom-profile-init-file), which (when called with no arguments) loads the
  # default profile.
  #
  # It is probably possible to hack around that, but let's see if we can make
  # the default profile work first: `doom doctor` may have additional problems
  # too hard to solve.

  emacsWithDoom = runCommand (lib.appendToName "with-doom" emacs).name {
    inherit (emacs) meta;
  } ''
    mkdir -p $out/bin
    ln -s ${emacs}/bin/* $out/bin/
    rm $out/bin/emacs-*
    ln -sf ${doomEmacs}/bin/doom-emacs $out/bin/emacs
    ln -sf ${doomEmacs}/bin/doomscript $out/bin/

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

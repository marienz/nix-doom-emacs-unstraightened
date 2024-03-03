{
  /* Your init.el. */
  doomInitFile ? null,
  /* Your packages.el. */
  doomPrivateModule ? null,
  /* Doom source tree. */
  doomSource,
  /* Emacs package to build against. */
  emacs,
  /* Whether to enable all default dependencies. Primarily useful for CI /
     testing. */
  full ? false,

  autoreconfHook,
  emacsPackagesFor,
  fetchFromGitHub,
  git,
  lib,
  linkFarm,
  makeWrapper,
  runCommand,
  runtimeShell,
  texliveBasic,
}:
let
  inherit (lib) optional optionalAttrs optionalString;

  # Step 1: determine which Emacs packages to pull in.
  #
  # Inputs: unpatched Doom, a DOOMDIR with the provided init.el and packages.el.
  # Outputs:
  # - Packages Doom normally loads using Straight (as json)
  # - modified packages.el that claims all packages are system-installed
  #
  # Uses Doom's CLI framework, which does not require anything else is installed
  # (not even straight).
  initialDoomDir = linkFarm "minimal-doom-dir" (
    [{ name = "cli.el"; path = ./cli.el; }]
    ++ optional (doomInitFile != null) { name = "init.el"; path = doomInitFile; }
    ++ optional (doomPrivateModule != null) { name = "packages.el"; path = doomPrivateModule; }
  );
  # Set DOOMLOCALDIR somewhere harmless to stop Doom from trying to create it
  # somewhere read-only.

  # (If this step breaks, add DEBUG=1 to make Doom more verbose.)

  # XXX this may need to be runCommandLocal just in case conditionals an init.el
  # / packages.el evaluate differently on build systems.
  doomIntermediates = runCommand "doom-intermediates"
    {
      env = {
        EMACS = lib.getExe emacs;
        DOOMDIR = initialDoomDir;
      };
    } ''
    mkdir $out
    export DOOMLOCALDIR=$(mktemp -d)
    ${runtimeShell} ${doomSource}/bin/doom dump-for-nix-build \
      ${optionalString full "--full"} -o $out
  '';

  doomPackageSet = lib.importJSON "${doomIntermediates}/packages.json";

  # URLs for a few packages used by Doom that have straight recipes but are not
  # in nixpkgs.
  extraUrls = {
    # Straight recipe from el-get
    font-lock-ext = "https://github.com/sensorflo/font-lock-ext.git";
    sln-mode = "https://github.com/sensorflo/sln-mode.git";
    # Straight recipe from emacsmirror-mirror
    nose = "https://github.com/emacsattic/nose.git";
    # In nixpkgs, but uses codeberg, for which nixpkgs uses fetchzip.
    # TODO: consider parsing origEPkg.src.url instead.
    spell-fu = "https://codeberg.org/ideasman42/emacs-spell-fu.git";
    tree-sitter-indent = "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git";
    undo-fu = "https://codeberg.org/ideasman42/emacs-undo-fu.git";
    undo-fu-session = "https://codeberg.org/ideasman42/emacs-undo-fu-session.git";
    visual-fill-column = "https://codeberg.org/joostkremers/visual-fill-column.git";
  };

  doomEmacsPackages = (emacsPackagesFor emacs).overrideScope (
    eself: esuper:
      let
        customPackages = {
          # Doom uses using emacs-straight/auctex, which still contains parts of
          # upstream's build system but does not contain all .in files, resulting
          # in a failed build if we attempt to use upstream's configure..
          auctex = esuper.trivialBuild {
            pname = "auctex";
            version = "1";
            meta = {
              description = "build auctex from emacs-straight for Doom";
            };
            # TODO: figure out why this is necessary (there may be a better
            # solution).
            preBuild = ''
              export HOME=$(mktemp -d)
            '';
          };
          # Doom lets Straight provide org-autoloads.el as an alternative for
          # org-loaddefs.el, and manually generates org-version.el.
          # I currently run Org's build system.
          # TODO: pass in ORGVERSION / GITVERSION (or provide a .git dir).
          org = esuper.trivialBuild {
            pname = "org";
            version = "1";
            meta = {
              description = "build org-mode from emacs-straight repo for Doom";
            };
            buildInputs = [ ];
            nativeBuildInputs = [ emacs makeWrapper ];
            # XXX this sticks stuff in $out/emacs/etc/org (datadir in default.mk)
            # that probably needs to go somewhere else.
            # Possibly same for $out/share/info/.
            configurePhase = ''
              echo "prefix = $out" > local.mk
              echo "lispdir = $out/share/emacs/site-lisp/org" >> local.mk
              make config
            '';
            buildPhase = ''
              runHook preBuild
              make
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              make install
              runHook postInstall
            '';
          };
          org-contrib = esuper.trivialBuild {
            pname = "org-contrib";
            version = "1";
            meta = {
              description = "build org-contrib from emacsmirror for Doom";
            };
            # Apply upstream fix for hard dependency on ess-custom.
            # Straight just seems to ignore the byte-compilation failure(?).
            patches = [ ./org-contrib-ob-stata-ess-optional.patch ];
            # HACK around sources being in lisp/, which trivialBuild does not
            # handle. Setting sourceDir would probably be more sane, but we
            # need the original one for a patch to apply.
            postPatch = ''
              cd lisp
            '';
          };
          sln-mode = esuper.trivialBuild {
            pname = "sln-mode";
            version = "1";
            meta = {
              description = "build sln-mode for doom with manual dependencies";
            };
            # Straight uses a recipe from el-get that specifiecs the font-lock-ext
            # dependency.
            buildInputs = [ eself.font-lock-ext ];
          };
          # Straight checks for git's presence at import time.
          # We could probably get by with feeding it /bin/true or similar,
          # but it seems not worth the risk.
          straight = esuper.trivialBuild {
            pname = "straight";
            version = "1";
            meta = {
              description = "build straight with Git dependency added for Doom";
            };
            nativeBuildInputs = [ git ];
          };
          # Nix uses a Melpa recipe that assumes the upstream CMake repo layout.
          # Doom uses emacsmirror and sets :files (:defaults "*").
          cmake-mode = esuper.trivialBuild {
            pname = "cmake-mode";
            version = "1";
            meta = {
              description = "build cmake-mode from emacsmirror for Doom";
            };
          };
        };

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
            # We have to specialcase ELPA packages pinned by Doom: Straight mirrors /
            # repackages them. Doom's pins assume that mirror is used (so we have to
            # use it), and replacing the source in nixpkgs's derivation will not work
            # (it assumes it gets a tarball as input).

            # TODO: check notmuch works correctly without notmuch-version.el

            isElpa = origEPkg != null && (origEPkg == esuper.elpaPackages.${name} or null || origEPkg == esuper.nongnuPackages.${name} or null);
            epkg =
              customPackages.${name}
                or (if origEPkg != null && !(p ? pin && isElpa)
              then origEPkg
              else
                assert lib.assertMsg
                  (isElpa || (p ? recipe && p ? pin) || extraUrls ? ${name})
                  "${name}: not in epkgs, not elpa, no recipe or not pinned";
                # Assume we can safely ignore (pre-)build unless we're actually
                # building our own package.
                assert lib.assertMsg (!(p ? recipe.pre-build)) "${name}: pre-build not supported";
                assert lib.assertMsg (!(p ? recipe.build)) "${name}: build not supported";
                # epkgs.trivialBuild takes an attrset, it does not support
                # mkDerivation's fixed-point evaluation (`finalAttrs`).
                # If it did, the buildInputs calculation should use it.
                esuper.trivialBuild ({
                  pname = name;
                  # src gets added below.
                  # version is required, but some other packages in nixpkgs just set 1.
                  version = "1";
                  meta = {
                    description = "trivial build for doom-emacs";
                  };
                  buildInputs = map (name: eself.${name}) reqlist;
                } // optionalAttrs (p ? recipe.files && p.recipe.files != { defaults = "*"; }) {
                  # HACK: files can contain :defaults, which splices in defaults.
                  # If files starts with :defaults, the entire thing gets
                  # misinterpreted as a proplist when exported to json.
                  # This currently only happens for `(:defaults "*")`, which we can
                  # safely ignore (skipping a few excludes).
                  postUnpack = ''
                    filteredSrc=$PWD/filteredSrc
                    mkdir $filteredSrc
                    pushd $sourceRoot
                    cp -r ${builtins.toString p.recipe.files} $filteredSrc
                    popd
                    sourceRoot=$filteredSrc
                  '';
              }));
            url =
              if (p.recipe.host or "") == "github" && p ? recipe.repo
              then "https://github.com/${p.recipe.repo}"
              else epkg.src.gitRepoUrl
                or (if isElpa then "https://github.com/emacs-straight/${name}"
              else extraUrls.${name}
                or (throw "${name}: cannot derive url from recipe ${p.recipe or "<missing>"}"));
            # Use builtins.fetchGit instead of nixpkgs's fetchFromGitHub because
            # fetchGit allows fetching a specific git commit without a hash.
            src = builtins.fetchGit (
              {
                inherit url;
                rev = p.pin;
                submodules = !(p.recipe.nonrecursive or false);
                # TODO: might need to pull ref from derivation.src if we're not pulling it from p.recipe?
                # Note Doom does have packages with pin + branch (or nonrecursive) set,
                # expecting to inherit the rest of the recipe from Straight.
              } // optionalAttrs (p ? recipe.branch) { ref = p.recipe.branch; }
              // optionalAttrs (p ? recipe.depth) { shallow = p.recipe.depth == 1; }
            );
            # Ignore dependency extraction errors because it fails for repos not
            # containing a "proper" package (no -pkg.el, no file with the right magic
            # header). These seem common enough to be not worth allowlisting.
            reqfile = runCommand "${name}-deps" { } ''
              ${lib.getExe emacs} -Q --batch --eval \
                "(progn
                   (require 'package)
                   (with-temp-buffer
                     (setq default-directory \"${src}\")
                     (dired-mode)
                     (let ((reqs (with-demoted-errors \"Extracting dependencies: %s\" (package-desc-reqs (package-dir-info)))))
                       (princ (json-encode (mapcar #'car (seq-remove (lambda (p) (apply #'package-built-in-p p)) reqs)))))))" \
                > $out
            '';
            reqjson = lib.importJSON reqfile;
            # json-encode encodes the empty list as null (nil), not [].
            reqlist = if reqjson == null then [ ] else reqjson;
          in
          if p ? pin
          then epkg.overrideAttrs { inherit src; }
          else epkg;
      in
      lib.mapAttrs makePackage doomPackageSet
  );

  emacsWithPackages = doomEmacsPackages.emacsWithPackages (epkgs: (map (p: epkgs.${p}) (builtins.attrNames doomPackageSet)));
in
emacsWithPackages

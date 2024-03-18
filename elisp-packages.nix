{
  emacs,
  eself,
  esuper,
  git,
  makeWrapper,
  writeText,
}:
{
  # Doom uses emacs-straight/auctex, which still contains parts of upstream's
  # build system but does not contain all .in files, resulting in a failed build
  # if we attempt to use upstream's configure.
  #
  # Use melpaBuild instead of trivialBuild because company-auctex installs as a
  # package (with a specified dependency) while trivialBuild does not include
  # the necessary package metadata to satisfy that dependency.
  auctex = esuper.melpaBuild {
    pname = "auctex";
    version = "1";
    meta = {
      description = "build auctex from emacs-straight for Doom";
    };
    # Most of auctex fails to byte-compile unless we do this.
    # TODO: figure out why this is necessary (there may be a better
    # solution).
    preBuild = ''
      export HOME=$(mktemp -d)
    '';
    # TODO: set this properly (melpa2nix requires it).
    commit = "unset";
    recipe = writeText "auctex-recipe" ''
      (auctex :fetcher github :repo "emacsmirror/auctex")
    '';
  };
  # Doom lets Straight provide org-autoloads.el as an alternative for
  # org-loaddefs.el, and manually generates org-version.el.
  # I currently run Org's build system.
  #
  # This does not hit the same dependency problem auctex does because org is a
  # built-in package, and the package.el machinery assumes that satisfies the
  # dependency (it is not aware of our shadowing it).
  #
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
    # Do not install files that shadow builtins and/or have undesirable side
    # effects if loaded.
    postUnpack = ''
      filteredSrc=$PWD/filteredSrc
      mkdir $filteredSrc
      cp $sourceRoot/straight*.el $filteredSrc
      sourceRoot=$filteredSrc
    '';
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
  # Doom uses a recipe with :files (:defaults "*"), which MELPA's package-build
  # rejects because it includes dotfiles
  # (https://github.com/melpa/package-build/pull/67).
  # Use a melpaBuild here so the package ends up in its own directory:
  # it uses that directory as a snippets directory, and using site-lisp/ as that
  # might go wrong.
  doom-snippets = esuper.melpaBuild {
    pname = "doom-snippets";
    version = "1";
    # melpa2nix requires that we set this. TODO: set correctly.
    commit = "unset";
    meta = {
      description = "trivial build of doom-snippets";
    };
    # The directories we want to match must be mode names: assume those are
    # sensibly named (they currently are).
    recipe = writeText "doom-snippets-recipe" ''
      (doom-snippets :fetcher github :repo "doomemacs/snippets"
                     :files (:defaults "*-mode"))
    '';
    packageRequires = [ eself.yasnippet ];
    # Stop all snippets from ending up on load-path via
    # normal-top-level-add-subdirs-to-load-path.
    # Avoids "default" in one of these from being mistaken for default.el.
    preBuild = ''
      for d in *-mode; do
        touch $d/.nosearch
      done
    '';
  };
  # TODO: clean up some more load-path clutter?
  #
  # The single biggest contributors are tree-sitter-langs (73) and ansible (36),
  # leaving 48 others.
  #
  # It looks like upstream has the same issue for both of these.
  # For tree-sitter-langs, we do use our own generated derivation (because elpa),
  # but the problematic directory is package-specific (queries/).
  # For ansible, we use upstream's derivation from melpaPackages.
  #
  # Since it looks like it's not breaking things and the number of entries added
  # to load-path is relatively modest I'm leaving this alone for now.
}

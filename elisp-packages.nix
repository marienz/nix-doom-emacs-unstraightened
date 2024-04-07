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

    # Finding ORGVERSION is a hack (based on the one in Doom).
    # TODO: set GITVERSION?
    configurePhase = ''
      echo "prefix = $out" > local.mk
      echo "lispdir = $out/share/emacs/site-lisp/org" >> local.mk
      echo "ORGVERSION = $(sed -ne 's/^;; Version: \([^\n-]\+\).*/\1/p' lisp/org.el)" >> local.mk
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
    packageRequires = [ eself.org ];
    sourceRoot = "source/lisp";
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
  # Fix /build/ leaking into byte-compiled files (patch accepted upstream).
  phpactor = esuper.phpactor.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      ./elisp-patches/0001-Do-not-call-locate-user-emacs-file-when-compiling.patch
    ];
  });
  # Make it byte-compile properly.
  code-review = esuper.code-review.overrideAttrs (attrs: {
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ git ];
  });
  # Make it byte-compile (see auctex)
  company-auctex = esuper.company-auctex.overrideAttrs (attrs: {
    preBuild = (attrs.preBuild or "") + ''
      export HOME=$(mktemp -d)
  '';
  });
  # Make it byte-compile.
  #
  # TODO ask upstream about missing evil dependency?
  # https://github.com/PythonNut/evil-easymotion/commit/fb7182625fcb1b1f7d43f69df620d98aa0f42a86
  # removed the dependency, I do not understand why.
  evil-easymotion = esuper.evil-easymotion.overrideAttrs (attrs: {
    buildInputs = attrs.buildInputs ++ [ eself.evil ];
  });

  # Other files that fail to byte-compile:
  # - rustic-flycheck, no flycheck dependency. Seems undesirable to force.
  # - stylus-mode, missing dependency on sws-mode(?)
  #   See also https://github.com/doomemacs/doomemacs/commit/f9feaec5bd75f4d997e0b07bc5c8b9177be20781
  # - xref-js2: upstream bug(?).
  #   Error: `add-to-list' can't use lexical var `words'; use `push' or `cl-pushnew'
  # - several others, looks like mostly missing (frequently optional) deps.
  # TODO: add a way of checking for these.
  # Currently I run:
  # set edir (grep 'export emacsWithPackages_siteLisp=' result/bin/.emacs-wrapped | cut -f2 -d=); comm -3 (fd '\.el$' $edir | sort | psub) (fd '\.elc$' $edir | sed -e 's:c$::' | sort | psub ) | grep -E -v -- '-(autoloads|pkg|theme).el$' | xargs grep -L 'no-byte-compile: t'

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

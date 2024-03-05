{
  emacs,
  eself,
  esuper,
  git,
  makeWrapper,
  writeText,
}:
{
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
  # Doom uses a recipe with :files (:defaults "*"), which MELPA's package-build
  # rejects because it includes dotfiles
  # (https://github.com/melpa/package-build/pull/67).
  # Use a melpaBuild here so the package ends up in its own directory:
  # it uses that directory as a snippets directory, and using site-lisp/ as that
  # might go wrong.
  #
  # XXX this results in all snippets directories being added to load-path...
  # This makes a mess (`(load "default")` ends up loading a snippet...).
  #
  # Does not look doom-snippets-specific, I also have
  #
  # /nix/store/...-emacs-packages-deps/share/emacs/site-lisp/elpa/ansible-20240212.325/snippets/text-mode/ansible/f5
  #
  # and so forth.
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
  };
}

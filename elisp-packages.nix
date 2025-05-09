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
  emacs,
  eself,
  esuper,
  git,
  lib,
  makeWrapper,
  stdenvNoCC,
  writableTmpDirAsHomeHook,
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
    nativeBuildInputs = [
      writableTmpDirAsHomeHook
    ];
  };
  # Doom lets Straight provide org-autoloads.el as an alternative for
  # org-loaddefs.el, and manually generates org-version.el.
  #
  # I currently run Org's build system to generate org-version.el.
  # But we need this org on load-path while byte-compiling packages that depend on it.
  # We accomplish that by using a melpaBuild for everything else.
  #
  # If we let org install itself, it ends up in an "org" subdir of site-lisp that is not on
  # load-path while byte-compiling dependent packages, causing those to pick up the built-in version
  # of org (causing problems at runtime).
  #
  # This ends up writing both org-autoloads.el and org-loaddefs.el, which combined provide the same
  # autoloads org-loaddefs.el has when using just Org's build system. I doubt all this is working as
  # intended, but the end result seems to work...
  org = esuper.melpaBuild {
    pname = "org";
    version = "1";
    meta = {
      description = "build org-mode from emacs-straight repo for Doom";
    };
    buildInputs = [ ];
    nativeBuildInputs = [
      emacs
      makeWrapper
    ];
    # Finding ORGVERSION is a hack (based on the one in Doom).
    # TODO: set GITVERSION?
    # datadir makes oc-csl find etc/csl and ox-odt find etc/styles.
    # org-odt-schema-dir stays nil because it looks for od-schema*.rnc which is not installed.
    # (Not sure if OpenDocument-schema-v1.3.rnc is misnamed or this file is not distributed...)
    configurePhase = ''
      echo "prefix = $out" > local.mk
      echo "datadir = $out/share/emacs/site-lisp/org/etc" >> local.mk
      echo "ORGVERSION = $(sed -ne 's/^;; Version: \([^\n-]\+\).*/\1/p' lisp/org.el)" >> local.mk
      make config
    '';
    preBuild = ''
      make autoloads
    '';
    postInstall = ''
      make install-etc install-info
    '';
  };
  org-contrib = esuper.melpaBuild {
    pname = "org-contrib";
    version = "1";
    meta = {
      description = "build org-contrib from emacsmirror for Doom";
    };
    packageRequires = [ eself.org ];
  };
  sln-mode = esuper.melpaBuild {
    pname = "sln-mode";
    version = "1";
    meta = {
      description = "build sln-mode for doom with manual dependencies";
    };
    # Straight uses a recipe from el-get that specifies the font-lock-ext
    # dependency.
    buildInputs = [ eself.font-lock-ext ];
  };
  # Straight checks for git's presence at import time.
  # We could probably get by with feeding it /bin/true or similar,
  # but it seems not worth the risk.
  straight = esuper.melpaBuild {
    pname = "straight";
    version = "1";
    meta = {
      description = "build straight with Git dependency added for Doom";
    };
    # Do not install files that shadow builtins and/or have undesirable side
    # effects if loaded.
    files = "(\"straight*.el\")";
    nativeBuildInputs = [ git ];
  };
  # Nix uses a Melpa recipe that assumes the upstream CMake repo layout.
  # Doom uses emacsmirror and sets :files (:defaults "*").
  cmake-mode = esuper.melpaBuild {
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
    meta = {
      description = "trivial build of doom-snippets";
    };
    # The directories we want to match must be mode names: assume those are
    # sensibly named (they currently are).
    files = ''(:defaults "*-mode")'';
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
  # Contains an extension containing debug.el that should not be on load-path.
  julia-snail = esuper.julia-snail.overrideAttrs (old: {
    preBuild =
      (old.preBuild or "")
      + ''
        for d in extensions/*; do
          touch $d/.nosearch
        done
      '';
  });
  # Rustic dropped its hard flycheck dependency upstream, but Doom is pinned to a revision
  # that still has it, causing errors at nativecomp time.
  # TODO: drop this once Doom catches up
  rustic = esuper.rustic.overrideAttrs (old: {
    packageRequires = old.packageRequires ++ [ eself.flycheck ];
  });
  # TODO: refactor our dependency-extraction so we can apply it selectively to packages we don't
  # generate the entire derivation for.
  #
  # nixpkgs deletes dapui.el because it is empty (only comments), triggering a compilation error.
  # Upstream deleted the file in
  # https://github.com/emacs-lsp/dap-mode/commit/438679755e880f2a662a63bc04da9e843257e248
  #
  # TODO: remove this if nixpkgs drops support for dap-mode that needed this.
  #
  # nixpkgs applies the fix conditionally, but since we set our version to "9999snapshot" we
  # unintentionally get counted as an old version.
  dap-mode = esuper.dap-mode.overrideAttrs (old: {
    preBuild = lib.replaceStrings [ "rm --verbose dapui.el" ] [ "" ] old.preBuild;
  });
  tree-sitter-langs =
    # Normally (outside nixpkgs), this package's tree-sitter-langs-build pulls a pre-compiled
    # grammar bundle from github. It also contains a build system to build that bundle from
    # submodules.
    #
    # Normally (inside nixpkgs), nixpkgs substitutes its own bundle for upstream's. It puts
    # upstream's version number (from melpa stable) in that bundle, which tree-sitter-langs compares
    # against its version number hardcoded as tree-sitter-langs--bundle-version.
    #
    # If Doom pins this package (which it does), that affects tree-sitter-langs--bundle-version but
    # not the version in the grammar bundle nixpkgs created. This causes tree-sitter-langs-build to
    # attempt to download its bundle and overwrite the nixpkgs one. This fails (with a download
    # error at build time and an attempt to write into the Nix store at runtime).
    #
    # Since the bundle version already does not match upstream's version anyway, take the easy way
    # out: patch that version number to match what nixpkgs put in the grammar bundle.
    let
      inherit (esuper.melpaStablePackages.tree-sitter-langs) version;
    in
    esuper.tree-sitter-langs.overrideAttrs (old: {
      postPatch =
        old.postPatch or ""
        + ''
          sed -i -e '/defconst tree-sitter-langs--bundle-version/ s/"[0-9.]*"/"${version}"/' \
            ./tree-sitter-langs-build.el
        '';
    });
  # mu4e-compat depends on mu4e, which (if I understand correctly) cannot be on melpa because it is
  # bundled with mu, and therefore mu4e-compat cannot have the dependency in its package-requires.
  # But it does not byte-compile without mu4e present. Add the dependency.
  mu4e-compat = esuper.melpaBuild {
    pname = "mu4e-compat";
    version = "9999snapshot1";
    meta = {
      description = "trivial build for doom-emacs";
    };
    packageRequires = [ eself.mu4e ];
  };
  # TODO: attempt to fix sly-stepper properly.
  # sly-stepper `require`s `sly-stickers`. That lives in sly's contribs subdirectory: it looks like
  # that is normally added to the load path by `sly-setup`, which Doom runs from after-init-hook.
  # But at byte compile time that subdirectory is not on the load path, breaking byte compilation.
  sly-stepper = esuper.melpaBuild {
    pname = "sly-stepper";
    version = "9999snapshot1";
    meta = {
      description = "trivial build for doom-emacs";
    };
    packageRequires = [ eself.sly ];
    ignoreCompilationError = true;
  };
  org-noter = esuper.org-noter.overrideAttrs (attrs: {
    # Nixpkgs conditionally patches an older version, which our "9999snapshot" version breaks.
    patches = [ ];
  });
  # Newer nixpkgs uses a melpa-build that requires a library with the same name of the package name.
  # Several wanderlust-related packages do not. emacsmirror fixed this: apply their fix.
  apel = esuper.apel.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      ./elisp-patches/apel-library.patch
    ];
  });
  flim = esuper.flim.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      ./elisp-patches/flim-library.patch
    ];
  });
  semi = esuper.semi.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      ./elisp-patches/semi-library.patch
    ];
  });
  wanderlust = esuper.wanderlust.overrideAttrs (attrs: {
    patches = (attrs.patches or [ ]) ++ [
      ./elisp-patches/wanderlust-library.patch
    ];
  });
  # Upstream renamed from opencl-mode to opencl-c-mode. melpa2nix requires single-file-package file
  # names match the package name. So rename the package (not the file, just in case someone loads it
  # explicitly).
  opencl-mode = esuper.opencl-c-mode;
  # reveal.js is not actually an ELisp package. Doom gets straight.el to install it,
  # then makes org-re-reveal use it as data.
  revealjs = stdenvNoCC.mkDerivation {
    pname = "revealjs";
    version = "9999snapshot1";
    buildPhase = ''
      siteDir=$out/share/emacs/site-lisp/revealjs
      mkdir -p $siteDir
      cp -r css dist js plugin $siteDir/
    '';
  };
  # Make it byte-compile properly.
  code-review = esuper.code-review.overrideAttrs (attrs: {
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [ git ];
  });
  # Make it byte-compile (see auctex)
  company-auctex = esuper.company-auctex.overrideAttrs (attrs: {
    nativeBuildInputs = (attrs.nativeBuildInputs or [ ]) ++ [
      writableTmpDirAsHomeHook
    ];
  });
  # This also needs a real $HOME. nixpkgs actually provides one but it's an Elpa package...
  # That also means esuper.auctex-cont-latexmk doesn't work here.
  auctex-cont-latexmk = esuper.melpaBuild {
    pname = "auctex-cont-latexmk";
    version = "9999snapshot1";
    packageRequires = [ eself.auctex ];
    nativeBuildInputs = [
      writableTmpDirAsHomeHook
    ];
  };
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
  #
  # To check for these: `doom-emacs --script build-helpers/byte-compile-check.el`

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

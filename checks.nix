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
  callPackages,
  emptyDirectory,
  lib,
  linkFarm,
  runCommand,
  testers,
  tmux,
  writeText,
  writeTextDir,

  doomSource,
  makeDoomPackages,
  toInit,
}:
let
  inherit (lib.generators) toPretty;
  inherit (callPackages ./build-helpers/full-init.nix { inherit doomSource; })
    doomDirWithAllModules doomDirWithAllModulesAndFlags;
  common = {
    doomLocalDir = "~/.local/share/nix-doom-unstraightened";
    experimentalFetchTree = true;
  };
  mkDoom = args: (makeDoomPackages (common // args)).doomEmacs;
  mkDoomDir = args: writeTextDir "init.el" (toInit args);
  minimalDoomDir = mkDoomDir { config.default = true; };
  doomTest = name: init: doomArgs: testers.testEqualContents {
    assertion = "name = ${name}; modules = ${toPretty {} init}; args = ${toPretty {} doomArgs};";
    expected = writeText "doom-expected" "Doom functions";
    # Runs Doom in tmux, waiting (by polling) until its window disappears.
    actual = runCommand "interactive" {
      # Read by tests.el.
      testName = name;
      nativeBuildInputs = [ tmux (mkDoom (doomArgs // {
        doomDir = linkFarm "test-doomdir" {
          "config.el" = ./tests.el;
          "init.el" = writeText "init.el" (toInit init);
        };
      })) ];
    } ''
      tmux new-session -s doom-testing -d
      tmux new-window -n doom-window doom-emacs
      for ((i = 0; i < 100; i++)); do
        tmux list-windows -a | grep -q doom-window || break
        sleep .1
      done
      tmux kill-session -t doom-testing
      '';
  };
in {
  minimal = mkDoom { doomDir = minimalDoomDir; };
  minimalEmacs = (makeDoomPackages (common // {
    doomDir = minimalDoomDir;
  })).emacsWithDoom;
  minimalExtraPackages = mkDoom {
    doomDir = minimalDoomDir;
    extraPackages = epkgs: [ epkgs.vterm epkgs.treesit-grammars.with-all-grammars ];
  };
  allModules = mkDoom { doomDir = doomDirWithAllModules; };
  allModulesAndFlags = mkDoom { doomDir = doomDirWithAllModulesAndFlags; };
  example = mkDoom { doomDir = ./doomdir; };
  example-without-loader = mkDoom {
    doomDir = ./doomdir;
    profileName = "";
  };
  interactive = doomTest "nix-profile" { config.default = true; } { };
  interactive-without-loader = doomTest "no-profile" { config.default = true; } { profileName = ""; };
  interactive-no-profile-hack = doomTest "no-profile" { config.default = true; } { noProfileHack = true; };

  cmake = doomTest "cmake" { lang.cc = true; } { };

  org-re-reveal = doomTest "org-re-reveal" { lang.org = [ "+present" ]; } { };

  # Various tests of module combinations.
  unpinned-org = doomTest "external-org" { app.rss = [ "+org" ]; } { };
  # Dependencies that require a module flag enabled and a different module or flag disabled.
  # Several are "+lsp and lsp without eglot" or "some flag and syntax without flymake".
  # TODO: Might be worth doing a full init.el build with just those two flags disabled.
  flycheck-guile = doomTest "nix-profile" { lang.scheme = [ "+guile" ]; checkers.syntax = true; } { };
  lsp-treemacs = doomTest "nix-profile" { ui.treemacs = [ "+lsp" ]; tools.lsp = true; } { };
  flycheck-eglot = doomTest "nix-profile" { tools.lsp = [ "+eglot" ]; checkers.syntax = true; } { };
  lsp-sourcekit-flycheck-swift = doomTest "nix-profile" {
    lang.swift = [ "+lsp" ];
    tools.lsp = true;
    checkers.syntax = true;
  } { };
  lsp-metals = doomTest "nix-profile" { lang.scala = [ "+lsp" ]; tools.lsp = true; } { };
  lsp-pyright = doomTest "nix-profile" { lang.python = [ "+lsp" "+pyright" ]; tools.lsp = true; } { };
  lsp-python-ms = doomTest "nix-profile" { lang.python = [ "+lsp" ]; tools.lsp = true; } { };
  org-roam2 = doomTest "nix-profile" { lang.org = [ "+roam2" ]; } { };
  flycheck-moonscript = doomTest "nix-profile" { lang.lua = [ "+moonscript" ]; checkers.syntax = true; } { };
  lsp-julia = doomTest "nix-profile" { lang.julia = [ "+lsp" ]; tools.lsp = true; } { };
  lsp-java = doomTest "nix-profile" { lang.java = [ "+lsp" ]; tools.lsp = true; } { };
  lsp-haskell = doomTest "nix-profile" { lang.haskell = [ "+lsp" ]; tools.lsp = true; } { };
  flycheck-stan = doomTest "nix-profile" { lang.ess = [ "+stan" ]; checkers.syntax = true; } { };
  lsp-dart = doomTest "nix-profile" { lang.dart = [ "+lsp" ]; tools.lsp = true; } { };
  ccls = doomTest "nix-profile" { lang.cc = [ "+lsp" ]; tools.lsp = true; } { };
  flycheck-irony = doomTest "nix-profile" { lang.cc = [ "+lsp" ]; checkers.syntax = true; } { };
  nerd-icons-dired = doomTest "nix-profile" { emacs.dired = [ "+icons" ]; } { };
  multiple-cursors = doomTest "nix-profile" { editor.multiple-cursors = true; } { };
  flx = doomTest "nix-profile" { completion.ivy = [ "+fuzzy" ]; } { };
  corfu-orderless = doomTest "nix-profile" { completion.corfu = [ "+orderless" ]; } { };
  flycheck-posframe = doomTest "nix-profile" { checkers.syntax = [ "+childframe" ]; } { };
  flyspell-correct-ivy = doomTest "nix-profile" { checkers.spell = [ "+flyspell" ]; completion.ivy = true; } { };
  flyspell-correct-helm = doomTest "nix-profile" { checkers.spell = [ "+flyspell" ]; completion.helm = true; } { };
  flyspell-correct-popup = doomTest "nix-profile" { checkers.spell = [ "+flyspell" ]; } { };

  extraPackages = doomTest "extraPackages" { config.default = true; } { extraPackages = epkgs: [ epkgs.vterm ]; };
}

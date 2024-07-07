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
  emacs,
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
  doomDirs = callPackages ./build-helpers/doomdirs.nix { inherit doomSource; };
  common = {
    doomLocalDir = "~/.local/share/nix-doom-unstraightened";
    experimentalFetchTree = true;
    emacs = emacs.override { withNativeCompilation = false; };
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
  doomBuildTest = init: mkDoom { doomDir = mkDoomDir init; };
in {
  minimal = mkDoom { doomDir = minimalDoomDir; };
  minimalEmacs = (makeDoomPackages (common // {
    doomDir = minimalDoomDir;
  })).emacsWithDoom;
  minimalExtraPackages = mkDoom {
    doomDir = minimalDoomDir;
    extraPackages = epkgs: [ epkgs.vterm epkgs.treesit-grammars.with-all-grammars ];
  };
  allModules = mkDoom { doomDir = doomDirs.allModules; };
  allModulesAndFlags = mkDoom { doomDir = doomDirs.allModulesAndFlags; };
  allModulesMostFlags = mkDoom { doomDir = doomDirs.allModulesMostFlags; };
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
  # flycheck-eglot needs flymake disabled.
  flycheck-eglot = doomBuildTest { tools.lsp = [ "+eglot" ]; checkers.syntax = true; };
  # roam2 needs +roam disabled.
  org-roam2 = doomBuildTest { lang.org = [ "+roam2" ]; };
  # nerd-icons-dired needs +dirvish disabled.
  nerd-icons-dired = doomBuildTest { emacs.dired = [ "+icons" ]; };
  # multiple-cursors needs :editor evil disabled.
  multiple-cursors = doomBuildTest { editor.multiple-cursors = true; };
  # flx needs +prescient disabled.
  flx = doomBuildTest { completion.ivy = [ "+fuzzy" ]; };
  # corfu pulls in unpinned orderless if vertico is disabled.
  corfu-orderless = doomBuildTest { completion.corfu = [ "+orderless" ]; };
  # flyspell can pull in one of three completion modules.
  flyspell-correct-ivy = doomBuildTest { checkers.spell = [ "+flyspell" ]; completion.ivy = true; };
  flyspell-correct-helm = doomBuildTest { checkers.spell = [ "+flyspell" ]; completion.helm = true; };
  flyspell-correct-popup = doomBuildTest { checkers.spell = [ "+flyspell" ]; };

  extraPackages = doomTest "extraPackages" { config.default = true; } { extraPackages = epkgs: [ epkgs.vterm ]; };
}

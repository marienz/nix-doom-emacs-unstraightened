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

  org-re-reveal = doomTest "org-re-reveal" { lang.org = [ "+present" ]; } { };

  # Various tests of module combinations.
  unpinned-org = doomTest "external-org" { app.rss = [ "+org" ]; } { };

  extraPackages = doomTest "extraPackages" { config.default = true; } { extraPackages = epkgs: [ epkgs.vterm ]; };
}

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
  inputs = {
    nixpkgs.url = "nixpkgs";
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs-stable.follows = "nixpkgs";
      };
    };
  };

  outputs = { doomemacs, nixpkgs, emacs-overlay, ... }: let
    systems = [ "x86_64-linux" ];
    perSystemPackages = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in {
      packages = perSystemPackages (pkgs:
        let
          common = {
            doomSource = doomemacs;
            # TODO: drop after NixOS 24.05 release.
            emacs = pkgs.emacs29;
            doomLocalDir = "~/.local/share/nix-doom-unstraightened";
          };
          pkgsWithEmacsOverlay = pkgs.extend emacs-overlay.overlays.package;
        in {
          # Current Doom + NixOS 23.11 requires emacs-overlay: Doom pins
          # emacs-fish-completion, which moved from gitlab to github recently
          # enough stable nixpkgs pulls it from the wrong source.
          doom-minimal = (pkgsWithEmacsOverlay.callPackages ./doom.nix (common // { doomDir = ./doomdirs/minimal; })).doomEmacs;
          doom-full = (pkgsWithEmacsOverlay.callPackages ./doom.nix (common // { full = true; doomDir = ./doomdirs/minimal; })).doomEmacs;
          doom-example = (pkgsWithEmacsOverlay.callPackages ./doom.nix (common // { doomDir = ./doomdirs/example; })).doomEmacs;
          doom-example-without-loader = (pkgsWithEmacsOverlay.callPackages ./doom.nix (common // {
            doomDir = ./doomdirs/example;
            profileName = "";
          })).doomEmacs;
        });
      overlays.default = final: prev:
        let
          pkgs = final.extend emacs-overlay.overlays.package;
          callPackages = args: (pkgs.callPackages ./doom.nix ({
            doomSource = doomemacs;
          } // args));
        in {
          doomEmacs = args: (callPackages args).doomEmacs;
          emacsWithDoom = args: (callPackages args).emacsWithDoom;
        };
      hmModule = import ./home-manager.nix {
        doomSource = doomemacs;
        emacsOverlay = emacs-overlay.overlays.package;
      };
    };
}

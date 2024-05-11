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
    systems.url = "github:nix-systems/default";
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        # These should be unused, but let's unset them to make that explicit.
        nixpkgs-stable.follows = "";
        nixpkgs.follows = "";
      };
    };
  };

  outputs = { self, systems, doomemacs, nixpkgs, emacs-overlay, ... }: let
    perSystemPackages = let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
    in
      f: eachSystem (system: f nixpkgs.legacyPackages.${system});
    # Hack to avoid pkgs.extend having to instantiate an additional nixpkgs.
    #
    # We need emacsPackagesFor from the overlay, but neither the overlay itself
    # (it only uses "super", not "self") nor us actually needs anything overlaid
    # on nixpkgs. So we can call the overlay and pass emacsPackagesFor through
    # directly instead of having pkgs.callPackage do it.
    emacsPackagesForFromOverlay = pkgs: (emacs-overlay.overlays.package {} pkgs).emacsPackagesFor;
    in {
      packages = perSystemPackages (pkgs:
        let
          common = {
            doomSource = doomemacs;
            # TODO: drop after NixOS 24.05 release.
            emacs = pkgs.emacs29;
            doomLocalDir = "~/.local/share/nix-doom-unstraightened";
            emacsPackagesFor = emacsPackagesForFromOverlay pkgs;
          };
          mkDoom = args: (pkgs.callPackages self (common // args)).doomEmacs;
        in {
          doom-minimal = mkDoom { doomDir = ./doomdirs/minimal; };
          doom-full = mkDoom {
            full = true;
            doomDir = ./doomdirs/minimal;
          };
          doom-example = mkDoom { doomDir = ./doomdirs/example; };
          doom-example-without-loader = mkDoom {
            doomDir = ./doomdirs/example;
            profileName = "";
          };
        });
      overlays.default = final: prev:
        let
          callPackages = args: (final.callPackages self ({
            doomSource = doomemacs;
            emacsPackagesFor = emacsPackagesForFromOverlay final;
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

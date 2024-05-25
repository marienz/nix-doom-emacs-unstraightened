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
    # Default to reusing the system's emacs package if it has nixpkgs in the system flake registry.
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
    doomFromPackages = pkgs: args: let
      # Hack to avoid pkgs.extend having to instantiate an additional nixpkgs.
      #
      # We need emacsPackagesFor from the overlay, but neither the overlay itself
      # (it only uses "super", not "self") nor us actually needs anything overlaid
      # on nixpkgs. So we can call the overlay and pass emacsPackagesFor through
      # directly instead of having pkgs.callPackage do it.
      inherit (emacs-overlay.overlays.package {} pkgs) emacsPackagesFor;
      mergedArgs = args // {
        inherit emacsPackagesFor;
        doomSource = doomemacs;
      };
    in
      pkgs.callPackages self mergedArgs;
    in {
      checks = perSystemPackages (pkgs: pkgs.callPackages ./checks.nix {
        makeDoomPackages = doomFromPackages pkgs;
      });
      packages = perSystemPackages (pkgs: {
        doom-example = (doomFromPackages pkgs {
          # TODO: drop after NixOS 24.05 release.
          emacs = pkgs.emacs29;
          doomDir = ./doomdirs/example;
          doomLocalDir = "~/.local/share/nix-doom-unstraightened";
        }).doomEmacs;
        doom-example-without-loader = (doomFromPackages pkgs {
          # TODO: drop after NixOS 24.05 release.
          emacs = pkgs.emacs29;
          doomDir = ./doomdirs/example;
          doomLocalDir = "~/.local/share/nix-doom-unstraightened";
          profileName = "";
        }).doomEmacs;
        # TODO: cache more packages, cache for more Emacsen.
        cachix-packages = pkgs.linkFarm "unstraightened-cachix-packages" {
          inherit doomemacs;
          full-emacs29 = (doomFromPackages pkgs {
            emacs = pkgs.emacs29;
            doomDir = ./doomdirs/minimal;
            doomLocalDir = "~/.local/share/nix-doom-unstraightened";
            full = true;
          }).doomEmacs.emacsWithPackages.deps;
        };
      });
      overlays.default = final: prev: {
        doomEmacs = args: (doomFromPackages final args).doomEmacs;
        emacsWithDoom = args: (doomFromPackages final args).emacsWithDoom;
      };
      hmModule = import ./home-manager.nix {
        inherit doomFromPackages;
      };
    };
}

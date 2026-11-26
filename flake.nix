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
    doomemacs = {
      url = "github:doomemacs/core";
      flake = false;
    };
    doomemacs-modules = {
      url = "github:doomemacs/modules";
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
    doomdir = {
      # This should be "path:./doomdir", but that causes issues with Lix.
      # As a workaround, point this at a branch that contains just the doomdir, at the top level.
      url = "github:marienz/nix-doom-emacs-unstraightened/doomdir";
      flake = false;
    };
  };

  outputs =
    {
      self,
      doomemacs,
      doomemacs-modules,
      nixpkgs,
      emacs-overlay,
      doomdir,
      ...
    }:
    let
      perSystemPackages =
        let
          eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
        in
        f: eachSystem (system: f nixpkgs.legacyPackages.${system});

      myLib = import ./lib.nix;

      doomFromPackages =
        pkgs: args:
        let
          # Hack to avoid pkgs.extend having to instantiate an additional nixpkgs.
          #
          # We need emacsPackagesFor from the overlay, but neither the overlay itself
          # (it only uses "super", not "self") nor us actually needs anything overlaid
          # on nixpkgs. So we can call the overlay and pass emacsPackagesFor through
          # directly instead of having pkgs.callPackage do it.
          inherit (emacs-overlay.overlays.package { } pkgs) emacsPackagesFor;
          mergedArgs = args // {
            inherit emacsPackagesFor;
            doomSource = doomemacs;
            doomModules = doomemacs-modules;
          };
        in
        pkgs.callPackages self mergedArgs;

      mkDoomDirs =
        pkgs: emacs:
        pkgs.callPackages ./build-helpers/doomdirs.nix {
          inherit emacs;
          doomSource = doomemacs;
          doomModules = doomemacs-modules;
        };

      homeModule = import ./home-manager.nix {
        inherit doomFromPackages;
        doomDirInput = doomdir;
      };
    in
    {
      checks = perSystemPackages (
        pkgs:
        pkgs.callPackages ./checks.nix {
          toInit = myLib.toInit nixpkgs.lib;
          makeDoomDirs = mkDoomDirs pkgs;
          makeDoomPackages = doomFromPackages pkgs;
        }
      );
      formatter = perSystemPackages (pkgs: pkgs.nixfmt);
      packages = perSystemPackages (
        pkgs:
        let
          doomFromFlake =
            args:
            doomFromPackages pkgs (
              {
                doomDir = doomdir;
                doomLocalDir = "~/.local/share/nix-doom-unstraightened";
              }
              // args
            );
          default = doomFromFlake { };
        in
        {
          doom-emacs = default.doomEmacs;
          doom-emacs-pgtk = (doomFromFlake { emacs = pkgs.emacs-pgtk; }).doomEmacs;
          emacs-with-doom = default.emacsWithDoom;
          doom-emacs-unset-profile = default.doomEmacs.override { profileName = ""; };
          doom-emacs-tangle = default.doomEmacs.override { tangleArgs = "."; };
        }
      );
      overlays.default = final: prev: {
        doomEmacs = args: (doomFromPackages final args).doomEmacs;
        emacsWithDoom = args: (doomFromPackages final args).emacsWithDoom;
      };
      inherit homeModule;
      # Original name used in our documentation.
      # `nix flake check` (as of Nix 2.24) prefers homeModule, so we provide both.
      hmModule = homeModule;

      lib = { inherit doomFromPackages; };
    };
}

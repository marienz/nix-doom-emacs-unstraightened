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
      systems,
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
          eachSystem = nixpkgs.lib.genAttrs (import systems);
        in
        f: eachSystem (system: f nixpkgs.legacyPackages.${system});

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
            inherit emacsPackagesFor toInit;
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

      # Convert a Nix expression to a `doom!` block suitable for init.el.
      #
      # Input: a nested attribute set.
      # The keys of the first level are categories (like `lang`).
      # The keys of the second level are module names (like `nix`).
      # The values are lists of module flags, or `true` for no flags.
      toInit =
        lib:
        let
          inherit (lib)
            concatLines
            concatStringsSep
            isList
            mapAttrsToList
            toPretty
            ;
        in
        attrs:
        concatLines (
          [ "(doom!" ]
          ++ (mapAttrsToList (
            cat: modules:
            (concatLines (
              [ (":" + cat) ]
              ++
                (mapAttrsToList (
                  mod: value:
                  if value == true then
                    mod
                  else if isList value then
                    "(${mod} ${concatStringsSep " " value})"
                  else
                    abort "${toPretty value} not supported"
                ))
                  modules
            ))
          ) attrs)
          ++ [ ")" ]
        );

      homeModule = import ./home-manager.nix {
        inherit doomFromPackages;
        doomDirInput = doomdir;
      };
    in
    {
      checks = perSystemPackages (
        pkgs:
        pkgs.callPackages ./checks.nix {
          toInit = toInit nixpkgs.lib;
          makeDoomDirs = mkDoomDirs pkgs;
          makeDoomPackages = doomFromPackages pkgs;
        }
      );
      formatter = perSystemPackages (pkgs: pkgs.nixfmt);
      packages = perSystemPackages (
        pkgs:
        let
          default = doomFromPackages pkgs {
            doomDir = doomdir;
            doomLocalDir = "~/.local/share/nix-doom-unstraightened";
          };
        in
        {
          doom-emacs = default.doomEmacs;
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

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

{ doomSource, emacsOverlay }:
{ config, options, lib, pkgs, ... }:

let
  cfg = config.programs.doom-emacs;
  inherit (lib) literalExpression mkEnableOption mkIf mkMerge mkOption types;
in {
  options = {
    programs.doom-emacs = {
      enable = mkEnableOption "Doom Emacs";

      emacs = mkOption {
        type = types.package;
        default = pkgs.emacs;
        defaultText = literalExpression "pkgs.emacs";
        example = literalExpression "pkgs.emacs29-pgtk";
        description = "The Emacs package to wrap.";
      };

      doomDir = mkOption {
        type = types.path;
        example = literalExpression "./doom";
        description = "The DOOMDIR to build from and bundle.";
      };

      doomLocalDir = mkOption {
        type = types.path;
        default = "${config.xdg.dataHome}/nix-doom";
        defaultText = literalExpression ''"''${config.xdg.dataHome}/nix-doom"'';
        example = literalExpression "~/.local/state/doom";
        description = ''
          DOOMLOCALDIR.

          `~` is expanded, but shell variables are not! Use `config.xdg.*`, not
          `XDG_DATA_*`.'';
      };

      profileName = mkOption {
        type = types.str;
        default = "nix";
        example = literalExpression "";
        description = "Doom profile. Set to the empty string to disable.";
      };

      noProfileHack = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Use a hack to make Doom use normal paths (relative to DOOMLOCALDIR).

          Has no effect if doomProfile is unset (set to the empty string).

          Currently not recommended: unset doomProfile instead;
        '';
      };

      provideEmacs = mkOption {
        type = types.bool;
        default = true;
        example = false;
        description = ''
          If enabled (the default), provide "emacs" (and "emacsclient", etc).
          If disabled, provide a "doom-emacs" binary.

          Disable this to install doom-emacs in parallel with vanilla Emacs.
        '';
      };

      finalEmacsPackage = mkOption {
        type = types.package;
        visible = false;
        readOnly = true;
        description = "The final Emacs-compatible package";
      };

      finalDoomPackage = mkOption {
        type = types.package;
        visible = false;
        readOnly = true;
        description = "The final doom-emacs package";
      };
    };
  };

  config = mkIf cfg.enable (mkMerge [
    (let
      # Hack to avoid pkgs.extend (see flake.nix).
      inherit (emacsOverlay {} pkgs) emacsPackagesFor;
      doomPackages = pkgs.callPackages ./doom.nix {
        inherit doomSource emacsPackagesFor;
        inherit (cfg) emacs doomDir doomLocalDir profileName noProfileHack;
      };
    in
      {
        programs.doom-emacs.finalDoomPackage = doomPackages.doomEmacs;
        programs.doom-emacs.finalEmacsPackage = doomPackages.emacsWithDoom;
      })
    {
      home.packages = [(
        if cfg.provideEmacs then cfg.finalEmacsPackage else cfg.finalDoomPackage
      )];
    }
    (mkIf (options.services ? emacs && cfg.provideEmacs) {
      services.emacs.package = cfg.finalEmacsPackage;
    })
  ]);
}

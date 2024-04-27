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
            emacs = pkgs.emacs29-pgtk;
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
    };
}

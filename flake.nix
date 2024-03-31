{
  inputs = {
    flake-parts.url = "flake-parts";
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

  outputs = inputs@{ self, doomemacs, nixpkgs, emacs-overlay, ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      perSystem = { self', inputs', system, pkgs, lib, ... }:
        let
          common = { doomSource = doomemacs; emacs = pkgs.emacs29-pgtk; };
        in
        {
          # Current Doom + NixOS 23.11 requires emacs-overlay: Doom pins
          # emacs-fish-completion, which moved from gitlab to github recently
          # enough stable nixpkgs pulls it from the wrong source.
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [ emacs-overlay.overlays.package ];
          };
          packages.doom-minimal = pkgs.callPackage ./doom.nix common;
          packages.doom-full = pkgs.callPackage ./doom.nix (common // { full = true; });
          packages.doom-example = pkgs.callPackage ./doom.nix (common // { doomDir = ./example; });
        };
    };
}

{
  inputs = {
    unstraightened.url = "path:../.";
    nixpkgs.follows = "unstraightened/nixpkgs";
    doomemacs.follows = "unstraightened/doomemacs";
    doomemacs-modules.follows = "unstraightened/doomemacs-modules";
  };

  outputs =
    {
      self,
      unstraightened,
      nixpkgs,
      ...
    }@inputs:
    let
      perSystemPackages =
        let
          # Just the ones with Cachix builds.
          eachSystem = nixpkgs.lib.genAttrs [
            "x86_64-linux"
            "aarch64-darwin"
          ];
        in
        f: eachSystem (system: f nixpkgs.legacyPackages.${system});

      # Duplicated from the main flake, as I do not want to expose this in lib.
      mkDoomDirs =
        pkgs: emacs:
        pkgs.callPackages ../build-helpers/doomdirs.nix {
          inherit emacs;
          doomSource = inputs.doomemacs;
          doomModules = inputs.doomemacs-modules;
        };
    in
    {
      packages = perSystemPackages (
        pkgs:
        let
          inherit (nixpkgs) lib;
          depBuilds =
            emacs:
            lib.flip lib.mapAttrs (mkDoomDirs pkgs emacs) (
              name: doomDir:
              (unstraightened.lib.doomFromPackages pkgs {
                inherit doomDir emacs;
                doomLocalDir = "~/.local/share/nix-doom-unstraightened";
                experimentalFetchTree = true;
              }).doomEmacs.emacsWithPackages.deps
            );
          emacsen =
            # Keep in sync with .github/workflows/cachix.yml
            (lib.genAttrs (
              [
                "emacs30"
                "emacs30-nox"
                "emacs30-gtk3"
                "emacs30-pgtk"
                "emacs31"
              ]
              ++ lib.optional pkgs.stdenv.hostPlatform.isDarwin "emacs30-macport"
            ) (name: pkgs.${name}))
            // {
              emacs-without-nativecomp = pkgs.emacs.overrideAttrs (old: {
                passthru = old.passthru // {
                  withNativeCompilation = false;
                };
              });
            };
        in
        lib.flip lib.mapAttrs' emacsen (
          name: emacs:
          let
            p = pkgs.linkFarm "cachix-${name}" (depBuilds emacs);
          in
          lib.nameValuePair p.name p
        )
      );
    };
}

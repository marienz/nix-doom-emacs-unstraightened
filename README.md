# nix-doom-emacs-unstraightened

`nix-doom-emacs-unstraightened` (referred to as "Unstraightened" below) builds
[Doom Emacs](https://github.com/doomemacs/doomemacs) using
[Nix](https://nixos.org) bundling a user configuration directory and the
dependencies specified by it. It is very similar to
[nix-doom-emacs](https://github.com/nix-community/nix-doom-emacs), but is
implemented differently.

## Status

[![CI](https://github.com/marienz/nix-doom-emacs-unstraightened/actions/workflows/ci.yml/badge.svg)](https://github.com/marienz/nix-doom-emacs-unstraightened/actions/workflows/ci.yml)

Tested and working on Linux, with emacs-overlay and Doom inputs updated
automatically. If you're reading this on GitHub, there should be a CI status
badge above: if CI is passing, Unstraightened installs an up-to-date version of
Doom Emacs and (almost) all module dependencies.

macOS is not covered by CI but was previously reported working. Please file an
issue if it does not work for you (and please comment on
https://github.com/marienz/nix-doom-emacs-unstraightened/issues/42 if you know
why CI is not working).

You may encounter "Cannot find Git revision" errors on Nix versions newer than
2.18.x (see [#14](https://github.com/marienz/nix-doom-emacs-unstraightened/issues/14)).
Try enabling `experimentalFetchTree` to work around this (see below).

Please report any issues.

## How to use

### Test run

1. Check out this repository
2. Optional: run `nix flake update nixpkgs` (Nix 2.19 or up) or `nix flake lock
   --update-input nixpkgs` (earlier versions). If `nixpkgs` is in the system
   registry (which it is by default on NixOS 24.05 and up) this will make
   Unstraightened reuse more dependencies already on your system.

> [!NOTE]
> Updating other inputs (with `nix flake update`) is not recommended. These
> inputs are automatically updated daily as long as tests pass. Updating
> manually may update to an incompatible version of Doom or Emacs packages.

3. `rm doomdir/*` (delete my example/test configuration)
4. Copy your Doom configuration into `doomdir`
5. Make sure all files are added to the Git index (`git add doomdir`)
6. Run `nix run .#doom-emacs`.

If this does not work, the "with flakes" setup below is unlikely to work either.
Please file an issue.

### With flakes

Add this flake as an input in `flake.nix`:

``` nix
inputs = {
  nix-doom-emacs-unstraightened.url = "github:marienz/nix-doom-emacs-unstraightened";
  # Optional, to download less. Neither the module nor the overlay uses this input.
  nix-doom-emacs-unstraightened.inputs.nixpkgs.follows = "";
};
```

If your Doom configuration lives in a different repository, add that as input
too:

``` nix
inputs = {
  doom-config.url = "...";
  doom-config.flake = false;
};
```

#### Home Manager

If you use [Home Manager](https://github.com/nix-community/home-manager), add
Unstraightened's Home Manager module in `flake.nix`:

``` nix
outputs = inputs @ { nixpkgs, home-manager, ... }: {
  homeConfigurations."username" = home-manager.lib.homeManagerConfiguration {
    modules = [
      inputs.nix-doom-emacs-unstraightened.homeModule
      ./home.nix
    ];
    extraSpecialArgs = { inherit inputs; };
  };
};
```

Configure it in `home.nix`:

``` nix
  programs.doom-emacs = {
    enable = true;
    doomDir = inputs.doom-config;  # or e.g. `./doom.d` for a local configuration
  };
```

There are a few other configurable options, see below.

If you set `services.emacs.enable = true`, that will run Unstraightened as well
(Unstraightened sets itself as `services.emacs.package`). Set
`programs.doom-emacs.provideEmacs = false` or override `services.emacs.package`
if you want a vanilla Emacs daemon instead.

> [!WARNING]
> Using the overlay described below with `programs.emacs.package` will not work
> correctly (see [HACKING](./HACKING.md) for details).

#### Overlay

If you don't use Home Manager or prefer not to use Unstraightened's Home Manager
module, add Unstraightened's overlay. Typically that means adding:

``` nix
nixpkgs.overlays = [ inputs.nix-doom-emacs-unstraightened.overlays.default ];
```

to a Home Manager or NixOS module.

The overlay adds two packages:

- To install Unstraightened in parallel with a normal Emacs, add:

  ``` nix
  (pkgs.doomEmacs {
    doomDir = inputs.doom-config;
    # If you stored your Doom configuration in the same flake, use
    #   doomDir = ./path/to/doom/config;
    # instead.
    doomLocalDir = "~/.local/share/nix-doom";
  })
  ```

  to your installed packages (see below for what `doomLocalDir` is for). This
  installs a binary named `doom-emacs`.

- To install Unstraightened as your default Emacs, use `pkgs.emacsWithDoom`
  instead of `pkgs.doomEmacs`. This installs a binary named `emacs` as well as
  `emacsclient` and other helpers (similar to [`emacsWithPackages` in
  nixpkgs](https://nixos.org/manual/nixos/stable/#module-services-emacs-adding-packages)).

### Without flakes

This is currently not explicitly supported, but should be possible (use
`pkgs.callPackages ./nix-doom-emacs-unstraightened`). PRs extending this part of
the documentation are welcome, as are (within reason) changes necessary to
support use without flakes.

### Options

`doomEmacs` and `emacsWithDoom` support the following options:

- `doomDir`: your configuration directory (also known as DOOMDIR, Doom private
  directory / module). Required.

- `doomLocalDir`: value Doom should use as `DOOMLOCALDIR`. Required, because by
  default Doom would use its source directory, which is read-only.

> [!NOTE]
> This supports `~` expansion but does **not** support shell variable expansion.
> Using `$XDG_DATA_HOME` will not work.

> [!NOTE]
> Because Unstraightened uses Doom's profile system, using the same value you
> used with vanilla Doom will not result in Unstraightened finding your files.
> See below.

- `emacs`: Emacs package to use. Defaults to `pkgs.emacs`.
  Use this to select different Emacs variants like `pkgs.emacs-pgtk`.

- `doomSource`: Doom source tree. Defaults to a flake input: overriding that
  input is probably easier than passing this.

- `extraBinPackages`: packages to add to `$PATH`. Defaults to Git, ripgrep and
  fd.

- `extraPackages`: Specify extra Emacs packages from nixpkgs to be available to
  Doom Emacs. Defaults to this function `epkgs: [ ]` (no extra packages).
  For example to include Emacs package `treesit-grammars.with-all-grammars`:
  `extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];`.

- `experimentalFetchTree`: fetch packages using `fetchTree`, which is more
  efficient but considered experimental in Nix (subject to changes which might
  break fetches).

- `tangleArgs`: When set, Unstraightened runs `doom +org tangle` in `doomDir`.
  See `doom +org tangle --help` for the arguments you can use here, and see the
  [Org-mode manual](https://orgmode.org/manual/Extracting-Source-Code.html) for
  more information.

- `emacsPackageOverrides`: Function passed to `(emacsPackagesFor
  emacs).overrideScope` (see [Nixpkgs
  manual](https://nixos.org/manual/nixpkgs/stable/#sec-emacs-config)). Runs
  after Unstraightened's own overrides have been applied. That means it can be
  used to fix problems with Unstraightened's automatically-generated
  derivations, by using `esuper.<name>.overrideAttrs`.

> [!NOTE]
> The `:config literate` module has no effect when using Unstraightened.
> Use `tangleArgs = "--all config.org"` instead.

> [!NOTE]
> Currently, this feature uses the version of Org-mode that comes with Emacs
> when tangling, not the version installed by Doom. This limitation could be
> lifted: file an issue if this limitation is a problem for you.

There are a few other settings but they are not typically useful. See the
source.

The Home Manager module supports the same options, as well as:

- `provideEmacs`: disable this to only provide a `doom-emacs` binary, not an
  `emacs` binary (that is: it switches from `emacsWithDoom` to `doomEmacs`). Use
  this if you want to install vanilla Emacs in parallel.
  
## Comparison to "normal" Doom Emacs

- Unstraightened updates Doom and its dependencies along with the rest of your
  Nix packages, removing the need to run `doom sync` and similar Doom-specific
  commands.

- Doom pins its direct dependencies, but still pulls the live version of some
  packages from MELPA or other repositories. Its pins are also applied to build
  recipes whose source is not pinned. This makes Doom installs not fully
  reproducible and can cause intermittent breakage.

  Unstraightened pulls these dependencies from nixpkgs or
  [emacs-overlay](https://github.com/nix-community/emacs-overlay). Pinning
  emacs-overlay pins all build recipes and packages not already pinned by Doom.

- Unstraightened stores your Doom configuration
  (`~/.doom.d`/`~/.config/doom`/`$DOOMDIR`) in the Nix store. This has
  advantages (the configuration's enabled modules always match available
  dependencies), but also some disadvantages (see known problems below).

- Unstraightened uses Doom's
  [profiles](https://github.com/doomemacs/doomemacs/tree/master/profiles) under
  the hood. This affects where Doom stores local state:

  | Variable | Doom | Unstraightened |
  |-|-|-|
  | `doom-cache-dir` | `$DOOMLOCALDIR/cache` | `~/.cache/doom` |
  | `doom-data-dir` | `$DOOMLOCALDIR/etc` | `~/.local/share/doom` |
  | `doom-state-dir` | `$DOOMLOCALDIR/state` | `~/.local/state/doom` |

  (Doom also stores some things in per-profile subdirectories below the above
  directories: the default profile name used by Unstraightened is `nix`,
  resulting in paths like `~/.cache/doom/nix`. All of these also respect the usual
  `XDG_*_DIR` environment variables.)

  When migrating from "normal" Doom, you may need to move some files around.

  If this bothers you, you can try setting `profileName = ""`. This makes
  Unstraightened use the usual paths (relative to `doomLocalDir`), but uses
  somewhat of a hack to do so (please report any issues observed).

## Comparison to `nix-doom-emacs`

- Unstraightened does not attempt to use straight.el at all. Instead, it uses
  Doom's CLI to make Doom export its dependencies, then uses Nix's
  `emacsWithPackages` to install them all, then configures Doom to use the
  "built-in" version for all its dependencies. This approach seems simpler to
  me, but time will have to tell how well it holds up.

- Unstraightened respects Doom's pins. I believe this is necessary for a system
  like this to work: Doom really does frequently make local changes to adjust to
  changes or work around bugs in its dependencies.

- Unstraightened is much younger. It is simpler in places because it assumes
  Emacs >=29. It probably still has some problems already solved by
  `nix-doom-emacs`, and it is too soon to tell how robust it is.

## Bugs

*Do not report bugs upstream*. If you think it's a bug in Doom, reproduce it
without Unstraightened first, or report it here first.

There are a few known current bugs and likely future bugs in Unstraightened:

### Pins can break

The way Unstraightened applies Doom's pins to Nix instead of straight.el build
recipes is a hack. Although it seems to work fairly well (better than I
expected), it will break at times.

If it breaks, it should break at build time, but I do not know all failure modes
to expect yet.

One likely failure mode is an error about Git commits not being present in the
upstream repository. To fix this, try building against a revision of the
`emacs-overlay` flake that is closer to the age of `doomemacs`. This is a
fundamental limitation: Doom assumes its pins are applied to `straight.el` build
recipes, while we use nixpkgs / emacs-overlay. If these diverge, our build
breaks.

Another possible problem is a package failing to build or run because one of its
dependencies is missing. Unstraightened currently uses dependencies from the
original (emacs-overlay) package. This is largely a performance optimization,
that can be revisited if it breaks too frequently.

### Saving Custom changes fails

Saving changes through Custom will not work, because `custom-file` is read-only.
I am open to suggestions for how this should work:

- Currently, `DOOMDIR/custom.el` is loaded, but changes need to be applied
  manually.
- If we set `custom-file` to a writable location, that fixes saving but breaks
  loading. If the user copies their custom-file out of their DOOMDIR to this
  location once, they are not alerted to changes they may want to copy back.
- If we try to use Home Manager, I would expect to hit the same problems
  and/or collisions on activation, but I have not experimented with this.

### Flag-controlled packages may be broken

Doom supports listing all packages (including ones pulled in by modules that are
not currently enabled). Unstraightened uses this to build-test them. However,
this does not include packages enabled through currently-disabled flags.

This is tricky because Doom seems to not support accessing supported flags
programmatically, and because some flags are mutually exclusive.

I may end up approximating this by checking in a hardcoded `init.el` with all
(or at least most) currently-available flags enabled.

### `doom doctor` fails with / complains about...

#### "Checking for stale elc files... File is missing"

```
> Checking for stale elc files...
x There was an unexpected runtime error
  Message: File is missing
  Details: ("Opening directory" "No such file or directory" "/home/marienz/.local/share/nix-doom/straight/build-30.1")

```

For now, just create the directory.

I would like to fix this but have not thought of the least messy way yet.

#### "Doom is installed in a non-standard location"

Ignore it.

Unstraightened uses `--init-directory`, as the doctor recommends.

#### "Found another Emacs config:"

Safe to ignore, for the same reason as the previous warning.

### tree-sitter grammars are not installed automatically

Install them manually by setting

``` nix
  extraPackages = epkgs: [ epkgs.treesit-grammars.with-all-grammars ];
```

(Or specify just the grammars you need by following the [example in
nixpkgs](https://github.com/NixOS/nixpkgs/blob/46cef0d5c1df9604fd53eebdaf69f31b74c13419/pkgs/applications/editors/emacs/elisp-packages/manual-packages/treesit-grammars/package.nix#L15),
but these are tiny so installing all of them is usually fine.)

This may be automated in the future (see [this
issue](https://github.com/marienz/nix-doom-emacs-unstraightened/issues/82)).

## Frequently Anticipated Questions

### How do I add more packages?

Add `(package! foo)` to `packages.el`.

Do not wrap emacsWithDoom in emacsWithPackages. See [HACKING](./HACKING.md) for
why this will not work.

The Home Manager option `extraPackages` is available to add extra Emacs packages from nixpkgs to Doom Emacs.
If this is not sufficient, please file an issue. 

### How do I add packages not in Emacs overlay?

Add `(package! foo :recipe ...)` to `packages.el`.

If there is a derivation in emacs-overlay or nixpkgs, it will use the same dependencies as the deriviation (which won't
always work if you pin to a sufficiently different version). If not, the dependencies are autodetected them from headers
in the package.

If the detected dependencies aren't sufficient, you'll have to add the package in Nix directly, with `extraPackages`.

```nix
              extraPackages = (
                epkgs: [
                  (epkgs.melpaBuild {
                    pname = <package-name>;
                    version = "9999snapshot1";
                    packageRequires = [ <dependencies> ];
                    src = fetchTree {...};
                  })
                ]
              );
```

See also [#70](https://github.com/marienz/nix-doom-emacs-unstraightened/issues/70).

If this is not sufficient, file an issue explaining what you're trying to do.

### What's wrong with `straight.el`?

`straight.el` is great, but its features are somewhat at odds with Nix:

- `straight.el` can fetch package build recipes and packages. We cannot use this
  from within Nix's build sandbox: we would need to build a system similar to
  how emacs-overlay updates elpa / melpa and get `straight.el` to use it.
- `straight.el` maintains a tree of mutable Git checkouts: you can edit these
  directly or use `straight.el` to maintain them. The Nix store is immutable so
  none of this will work.
- `straight.el` can build packages, but so can nixpkgs / emacs-overlay.

Doom heavily uses `straight.el` during `doom sync`, but it does not use it at
all at startup and barely uses it after that. Since we're replacing `doom sync`
in its entirety, bypassing `straight.el` seems simpler than trying to use it
just for package builds.

### Unstraightened seems to use `package.el`. Isn't that bad?

Doom's FAQ offers [several arguments against
`package.el`](https://github.com/doomemacs/doomemacs/blob/master/docs/faq.org#why-does-doom-use-straightel-and-not-packageel).
They boil down to two problems, neither of which applies to Unstraightened:

- `package.el` always builds from head: no rollback, no pinning, no
  reproducibility, no way to override the package source used. Unstraightened
  does not use `package.el` to fetch packages: it leaves that to Nix. We can
  handle pinning there, and Nix flakes add further reproducibility and rollback
  beyond what Doom's pins offer.
- `package.el` can be slow to initialize. Doom normally speeds up startup by
  combining autoloads from all installed packages into one file. Because
  `package.el` produces autoload files much like `straight.el` does, and we're
  loading everything from the immutable Nix store, we can apply exactly the same
  approach to `package.el`. Unstraightened startup performance should be about
  the same as vanilla Doom.

### It's so slow to build!

Parallel builds should help (set Nix's `max-jobs` to something greater than 1),
but it is a bit slow.

There are a few issues:

- Unstraightened uses
  [IFD](https://nixos.org/manual/nix/stable/language/import-from-derivation) to
  determine packages to install and to determine package dependencies for
  packages not in emacs-overlay. Especially the latter is slow.

- Doom (currently) [does not native-compile ahead of
  time](https://github.com/doomemacs/doomemacs/issues/6811), but Unstraightened
  (or nixpkgs, really), does.

  - It should be possible to disable nativecomp and/or move it to runtime, but
    see the next point...

- Unstraightened's packages should be cacheable, but I don't have that set up
  yet.

  - In particular, Unstraightened's generated derivations for elisp packages do
    not depend on the exact Doom source or configuration they're generated from.
    They depend on the pinned version and Emacs binary used to build them, but
    not much else. So it should be possible to build a Doom configuration with
    just a few modules enabled using commonly-used versions of Emacs from CI and
    push the results to a binary cache like https://cachix.org/.

### Required disclaimer

This is not an officially supported Google product. It is a personal [side
project](https://opensource.google/documentation/reference/releasing).

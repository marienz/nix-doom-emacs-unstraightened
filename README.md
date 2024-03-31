# nix-doom-emacs-unstraightened

`nix-doom-emacs-unstraightened` (referred to as "Unstraightened" below) builds
`doom-emacs`, bundling a user configuration directory and the dependencies
specified by it. It is very similar to
[nix-doom-emacs](https://github.com/nix-community/nix-doom-emacs), but is
implemented differently.

## How to use

TODO

## Comparison to "normal" Doom Emacs

- Unstraightened updates Doom and its dependencies along with the rest of your
  Nix packages, removing the need to run `doom sync` and similar Doom-specific
  commands.

- Doom pins most of its direct dependencies, but still pulls the live version of
  many packages from MELPA or other repositories. Its pins are also applied to
  build recipes whose source is not pinned. This makes Doom installs
  non-reproducible and can cause intermittent breakage.

  Unstraightened pulls these dependencies from nixpkgs or
  [emacs-overlay](https://github.com/nix-community/emacs-overlay), which can be
  pinned.

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
  resulting in paths like ~/.cache/doom/nix. All of these also respect the usual
  `XDG_*_DIR` environment variables.)

  When migrating from "normal" Doom, you may need to move some files around.

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

## Known problems

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

### Saving Custom changes fails

Saving changes through Custom will not work, because `custom-file` is read-only.
I am open to suggestions for how this should work:

- Currently, `DOOMDIR/custom.el` is loaded, but changes need to be applied
  manually.
- If we set `custom-file` to a writable location, that fixes saving but breaks
  loading. If the user copies their custom-file out of their DOOMDIR to this
  location once, they are not alerted to changes they may want to copy back.
- If we try to use home-manager, I would expect to hit the same problems
  and/or collisions on activation, but I have not experimented with this.

### `php` module without `+lsp` breaks the build

Enabling the `php` module without enabling the `+lsp` flag currently breaks the
build with:

```
     > Doom profile contains a forbidden reference to /build/
```

This is triggered by
[emacs-php](https://github.com/emacs-php/phpactor.el/blob/8733fef84b458457c1bfd188cfb861fc3150ee1c/phpactor.el#L71-L72)
determining the location of the user's Emacs directory at byte-compile time. I
do not know why it does this yet (it seems undesirable).

Upstream recommends migrating to LSP.

### Other flag-controlled packages may be broken

Doom supports listing all packages (including ones pulled in by modules that are
not currently enabled). Unstraightened uses this to build-test them. However,
this does not include packages enabled through currently-disabled flags.

This is tricky because Doom seems to not support accessing supported flags
programmatically, and because some flags are mutually exclusive.

I may end up approximating this by checking in a hardcoded `init.el` with all
(or at least most) currently-available flags enabled.

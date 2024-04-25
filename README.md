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

### Flag-controlled packages may be broken

Doom supports listing all packages (including ones pulled in by modules that are
not currently enabled). Unstraightened uses this to build-test them. However,
this does not include packages enabled through currently-disabled flags.

This is tricky because Doom seems to not support accessing supported flags
programmatically, and because some flags are mutually exclusive.

I may end up approximating this by checking in a hardcoded `init.el` with all
(or at least most) currently-available flags enabled.

### Some pins may not be applied

Doom
[mentions](https://github.com/doomemacs/doomemacs/blob/9620bb45ac4cd7b0274c497b2d9d93c4ad9364ee/modules/ui/treemacs/packages.el#L6)
some packages "have no `:pin` because they're in the same repo".

Doom assumes that if it pins `treemacs`, that pin applies to other packages
built from the same Git repository (like `treemacs-evil`). That comes somewhat
naturally to straight.el (since it only checks out each repository once), but it
does not come naturally to Nix (since it builds each package fully
independently).

I think I will be able to fix this but I haven't implemented it yet.

## Frequently Anticipated Questions

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

  - The dependency data probably gets garbage-collected, making subsequent
    evaluation slow even if nothing changed. I intend to make the installed
    packages depend on this data to work around this, but I have not implemented
    it yet.

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

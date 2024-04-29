# Internals/design notes

## Why [profiles](https://github.com/doomemacs/doomemacs/tree/master/profiles)?

Because the profile loader runs early enough we can set `doom-profile-data-dir`
(where the generated profile is stored and loaded from) outside `DOOMLOCALDIR`
relatively cleanly. Not using the "global" profile is a largely unintended side
effect, but the changes to other paths made seem largely reasonable so for now
I'm sticking with it.

After the profile loader, the next point we would get control is `doom-start.el`
loading `init.el` from `doom-user-dir`. We currently point `doom-user-dir` into
the store (see below): setting `doom-profile-data-dir` and `doom-profile-dir`
from there (by prepending to the user's `init.el`) would probably also work.
This seems a bit questionable because `doom-profile-dir` is set with `defconst`:
its const-ness is not enforced but I'd prefer not to take advantage of that. I
also have not checked if writing the profile would work out of the box with this
approach.

`noProfileHack` unsets `DOOMPROFILE` from the profile loader. This feels like a
hack, but it does get us the usual `DOOMLOCALDIR`-relative `doom-cache-dir` and
friends back.

## Why put `doom-user-dir`/`DOOMDIR` in the Nix store?

Doom forces my hand. I would prefer for just `packages.el` and possibly
`init.el` to live in the store, but splitting out where those are loaded from
looks non-trivial.

Doom uses `doom-user-dir` as the path to a special module (`:user`).

At profile generation time, this is how `packages.el` gets loaded. We want our
generated `packages.el` to take effect, so we want the `:user` module's path to
be a store path when generating the profile.

Paths to all modules are embedded in the generated profile and used to
initialize `doom-modules` at runtime. Among other things, this controls where
the user's `config.el` is loaded from (it's loaded along with module `config.el`
files). So (without further hacks) the path to `packages.el` at build time and
`config.el` at runtime are the same.

(And even if that wasn't the case, functions like `doom/help-packages` load
`packages.el`, and I currently expect less overall confusion if that loads our
generated `packages.el`, not the original one. So I do think we want the `:user`
module loaded from the store.)

In several places, Doom assumes (at runtime) that `doom-user-dir` and the path
to the `:user` module are the same. This is mostly in functions like
`doom/help-packages` and `doom-module-from-path` that map paths back to modules.

Combine all that and I think consistently having `doom-user-dir` and the `:user`
module live in the Nix store is the least bad option.

This does break things that write to DOOMDIR at runtime. `custom-file` is an
obvious example, but there are probably a few more.

## `programs.emacs.package` / nesting emacsWithPackages

Home Manager's `programs.emacs` wraps its Emacs package with emacsWithPackages.
We don't work as an input to emacsWithPackages.

First bug: emacsWithPackages writes a site-start that loads
`$emacs/share/site-lisp/site-start` first. That is: it assumes the emacs package
it wraps has its own site-start. That's true if it's an actual emacs but at
first glance might also break if it's another emacsWithPackages, because its
site-start goes in a separate `emacs-packages-deps` derivation (I didn't test
this further).

We could fix that (by adding a trivial site-start.el of our own), but there's a
second bug: when Doom loads its profile, it overwrites `load-path`. This defeats
the purpose of having that outer emacsWithPackages in the first place.

During normal interactive startup, the second bug masks the first: site-start
gets loaded from the Doom profile's load path, skipping the outer
emacsWithPackages entirely. So at first glance the Home Manager `programs.emacs`
module will seem to work...

During non-interactive startup, the first bug surfaces. The easiest way of
triggering this is `doom` cli, which fails with:

```
Unexpected error in Doom’s core: "/nix/store/3hr4amd670vbf5h1w1jw18y3a9hv1689-source/lisp/doom-cli.el", (file-missing "Cannot open load file" "No such file or directory" "/nix/store/7vvp8axf8h4qrx7mj3mh1dsxj80393k2-emacs-pgtk-with-doom-29.3/share/emacs/site-lisp/site-start")
```

Setting DEBUG=1 makes it more obvious where this fails (doom-cli.el loads
site-start).

Non-interactive use of emacs also seems to trigger this: in particular it breaks
flycheck of elisp code with a similar error message about site-start.

;;; cli.el -*- lexical-binding: t; -*-

;; We skip Doom's normal install and initialization.
(require 'straight)

(defadvice! nix-doom-skip-core-packages (orig-fn &rest args)
  "HACK: don't install straight and core packages.

`doom-initialize-core-packages' would no-op out if
`straight-recipe-repositories' is set, but we do not want to set
it. Just skip it entirely."
  :override #'doom-initialize-core-packages
  t)

(defcli! build-profile-for-nix-build ()
  "Write a Doom profile."
  ;; HACK: this initializes enough of straight (particularly
  ;; straight--build-cache) to make Doom work (I hope...).
  (straight-prune-build-cache)
  (doom-profile-generate))

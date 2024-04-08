;;; pre-init.el -*- lexical-binding: t; -*-

;; Added to init.el by nix-doom-emacs-unstraightened

(defadvice! nix-doom-skip-core-packages (&rest _)
  "HACK: don't install straight and core packages.

`doom-initialize-core-packages' would no-op out if
`straight-recipe-repositories' is set, but we do not want to set
it. Just skip it entirely."
  :override #'doom-initialize-core-packages
  (doom-log "Assuming straight was already initialized"))

;; nix-doom-emacs-unstraightened additions end here.
;; Original init.el follows.

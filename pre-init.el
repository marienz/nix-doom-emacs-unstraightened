;;; pre-init.el -*- lexical-binding: t; -*-

;; Added to init.el by nix-doom-emacs-unstraightened

(defadvice! nix-doom-skip-core-packages (&rest _)
  "HACK: don't install straight and core packages.

`doom-initialize-core-packages' would no-op out if
`straight-recipe-repositories' is set, but we do not want to set
it. Just skip it entirely."
  :override #'doom-initialize-core-packages
  (doom-log "nix-doom-emacs-unstraightened overriding core package init")
  ;; doom-initialize-core-packages normally registers recipes, which loads the
  ;; build cache by side effect, which leaves straight--build-cache available
  ;; afterwards. Doom assumes this cache is available, so force a load here.
  (straight--load-build-cache))

(after! doom-packages
  (setq straight-base-dir unstraightened--straight-base-dir))

;; nix-doom-emacs-unstraightened additions end here.
;; Original init.el follows.

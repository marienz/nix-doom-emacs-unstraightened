;; Prefer a few more built-in packages.
;;
;; This is not intended to affect Doom directly (it generally does not pin
;; transitive dependencies like these), but to stop other nixpkgs packages from
;; pulling them in. For example, `evil' depends on nadvice, but current versions
;; of Emacs have that built-in: nixpkgs does not know that, but thanks to IFD we
;; do.
(package! cl-generic :built-in 'prefer)
(package! nadvice :built-in 'prefer)

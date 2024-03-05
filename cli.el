;;; cli.el -*- lexical-binding: t; -*-

(require 'json)
(doom-require 'doom-cli 'packages)

(defcli! dump-for-nix-build
    ((output-directory ("-o" dir) "Directory to dump into.")
     (&flag full? ("--full")))
  "Dump intermediates for nix-doom-emacs-unstraightened."
  (let* ((packages (doom-package-list full?))
         ;; For built-in packages, the :ignore property is the location of the
         ;; built-in library, which is a Nix store path. We do not want that
         ;; path to escape: avoid it by just filtering ignored packages here.
         (packages (seq-remove (lambda (p) (plist-get (cdr p) :ignore)) packages))
         ;; For recipes with :files, print it to a string before json-encode.
         ;; Otherwise it is serialized as a plist if it starts with :defaults.
         ;; We either ignore this or pass it to melpa2nix in a recipe.
         (packages
          (mapcar (lambda (p)
                    (let* ((plist (cdr p))
                           (recipe (plist-get plist :recipe))
                           (files (plist-get recipe :files)))
                      (when files
                        (setcdr p
                                (plist-put plist :recipe
                                           (plist-put recipe :files
                                                      (prin1-to-string files)))))
                      p))
                  packages))
         (json (json-encode packages))
         (json-path (expand-file-name "packages.json" output-directory)))
    (write-region json nil json-path)))

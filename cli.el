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
         (json (json-encode packages))
         (json-path (expand-file-name "packages.json" output-directory)))
    (write-region json nil json-path)))

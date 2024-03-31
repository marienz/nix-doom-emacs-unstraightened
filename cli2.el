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

(defcli! build-profile-loader-for-nix-build
  ((profile-name ("-n" form) "Profile name.")
   (profile-directory ("-p" dir) "Profile data directory.")
   (profile-doom-dir ("-d" dir) "DOOMDIR"))
  "Write Doom's profile loader."
  (let ((new-profiles `((,profile-name
                         (user-emacs-directory . ,doom-emacs-dir)
                         (doom-profile-data-dir . ,profile-directory)
                         ("DOOMDIR" . ,profile-doom-dir)))))
    (doom-profiles-save new-profiles)))

;; Doom runs this with package.el activated, but suppresses activation during
;; normal startup. Store the side effects of activation in the profile to avoid
;; (slow) package activation during normal startup.
;;
;; package-activate-1 does:
;; - Load autoloads. Duplicated by generate-unstraightened-autoloads.
;; - Add to load-path. Doom already stores load-path.
;; - Add Info node. TODO.
;; - Add name to package-activated-list. Maybe serialize that?

(defun generate-unstraightened-autoloads ()
  "Like doom-profile--generate-package-autoloads but for package.el."
  (doom-autoloads--scan
   (mapcar (lambda (s)
             (format "%s.el"
                     (package--autoloads-file-name (package-get-descriptor s))))
           (seq-difference package-activated-list
                           (mapcar #'intern-soft
                                   doom-autoloads-excluded-packages)))
   doom-autoloads-excluded-files
   'literal))

(add-to-list
 'doom-profile-generators
 '("90-loaddefs-unstraightened.auto.el" . generate-unstraightened-autoloads))

(defcli! build-profile-for-nix-build ()
  "Write a Doom profile."
  ;; HACK: this initializes enough of straight (particularly
  ;; straight--build-cache, which doom-profile--generate-package-autoloads hits)
  ;; to make Doom work.
  ;;
  ;; TODO: remove doom-profile--generate-package-autoloads?
  ;; Because there are no straight-built packages, it generates an empty file.
  (straight-prune-build-cache)
  (doom-profile-generate))

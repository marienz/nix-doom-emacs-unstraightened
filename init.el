;;; init.el -*- lexical-binding: t; -*-

;; Copyright 2024 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Extra initialization code for nix-doom-emacs-unstraightened, run before the
;; normal `init.el'.

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
  (require 'straight)  ;; straight-load-build-cache is not autoloaded.
  (straight--load-build-cache))

(after! doom-packages
  (setq straight-base-dir "@straightBaseDir@"))

(defadvice! unstraightened-profile-init-file (&optional profile-id version)
  "Return unstraightened's profile init file.

`doom-profile-init-file' locates the profile relative to `doom-data-dir', but
nix-doom-emacs-unstraightened keeps its profile in a different location.
Override `doom-profile-init-file' to confirm it is called to get the default
or unstraightened profile (erroring out otherwise), then return the custom path.

Returning the unstraightened profile if the default profile is
requested makes `doom doctor' work."
  :override #'doom-profile-init-file
  (let ((my-profile "@profileName@"))
    (unless (or (null profile-id)
                (and (not (string-empty-p my-profile))
                     (string-equal profile-id my-profile)))
      (error "Accessing other profiles from Unstraightened is unsupported."))
  (unless (null version)
    (error
     "Accessing other profile versions from Unstraightened is unsupported."))
  (file-name-concat
   doom-profile-dir (format "init.%d.elc" emacs-major-version))))

;; Doom adds a minor mode that makes flycheck-mode's emacs subprocess initialize
;; Doom. Extend this to set our profile dir before it does so.
(setq-hook! +emacs-lisp--flycheck-non-package-mode
  flycheck-emacs-lisp-check-form
  (prin1-to-string `(progn
                      (setq doom-profile-data-dir ,doom-profile-data-dir)
                      ,(read flycheck-emacs-lisp-check-form))))

;; Load the user's init.el.
(load "@userInit@" nil 'nomessage)

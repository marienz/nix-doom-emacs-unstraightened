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

@maybe-set-profile-dir@

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
  (setq straight-base-dir "@straight-base-dir@"))

;; TODO: remove if Doom accepts https://github.com/doomemacs/doomemacs/pull/7849
(defadvice! nix-doom-configs-without-git (package)
  "Override to use ripgrep instead of git."
  :override #'doom--help-package-configs
  (let ((default-directory doom-emacs-dir))
    (split-string
     (cdr (doom-call-process
           "rg" "--no-heading" "--line-number" "--iglob" "!*.org"
           (format "%s %s($| )"
                   "(^;;;###package|\\(after!|\\(use-package!)"
                   package)))
     "\n" t)))

;; Load the user's init.el.
(load "@user-init@" nil 'nomessage)

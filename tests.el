;; -*- lexical-binding: t; -*-

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

;; Config file for integration tests.
;;
;; This tries to hook into startup as late as possible, write a sign of life
;; (currently a string written to $out), and then exits.

(defun test-noop ())

(defun test-no-profile ()
  (unless (or (null doom-profile)
              (equal (doom-profile-key t t) (doom-profile-key doom-profile t)))
    (error "doom-profile should be unset or default, is %s" doom-profile)))

(defun test-nix-profile ()
  (unless (and doom-profile
               (equal (car (doom-profile-key doom-profile t)) "nix"))
    (error "non-nix doom-profile %s" doom-profile)))

(defun test-external-org ()
  "Test org can be loaded and it's not built-in."
  (require 'org)
  (let ((path (find-library-name "org")))
    (unless (string-search "/site-lisp/" path)
      (error "org-mode probably built-in: %s" path))))

(defun test-org-re-reveal ()
  "Test org-re-reveal can find reveal.js."
  (require 'ox)
  (require 'org-re-reveal)
  (unless (string-search "/site-lisp/revealjs" org-re-reveal-root)
    (error "org-re-reveal does not find our revealjs: %s" org-re-reveal-root)))

(defun test-cmake ()
  "Test cmake-mode autoloads are loaded."
  (unless (functionp 'cmake-mode)
    (error "cmake-mode not available")))

(defun test-doom ()
  (let* ((out (getenv "out"))
         (test (intern-soft (format "test-%s" (getenv "testName"))))
         (result (condition-case err
                     (funcall test)
                   (error
                    (format "%s failed: %s" test err))
                   (:success
                    "Doom functions"))))
    (write-region result nil out nil nil nil 'mustbenew))
  (kill-emacs))

(defun test-extraPackages ()
  (require 'vterm))

(defun test-package-activated-list ()
  "Test package-activated-list is set."
  ;; straight could be any other package pulled in by a minimal Doom config.
  (unless (memq 'straight package-activated-list)
    (error "straight not on package-activated-list: %s"
           package-activated-list)))

(defun test-auto-mode-alist-has-go ()
  (unless (assoc "\\.go\\'" auto-mode-alist)
    (error ".go not on auto-mode-alist")))

(defun test-lsp-use-plists ()
  (require 'lsp-nix)
  (unless lsp-use-plists
    (error "lsp-use-plists nil"))
  (unless lsp-nix-plist-value-when-compiled
    (error "lsp-nix compiled without plists")))

(defun test-lsp-use-hashtables ()
  (require 'lsp-nix)
  (when lsp-use-plists
    (error "lsp-use-plists set"))
  (when lsp-nix-plist-value-when-compiled
    (error "lsp-nix compiled with plists")))

(add-hook 'doom-after-init-hook 'test-doom)

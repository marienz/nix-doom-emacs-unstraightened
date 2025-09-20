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

(doom! :completion
       vertico

       :ui
       doom
       doom-dashboard
       modeline
       nav-flash
       ophints
       (popup +defaults)
       window-select

       :editor
       evil

       :emacs
       undo

       :term
       eshell
       vterm

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       emacs-lisp
       (nix +lsp)

       :tools
       (lsp +eglot)

       :config
       (default +bindings +smartparens))

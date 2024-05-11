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
       (company +childframe)
       (corfu +orderless +icons +dabbrev)
       vertico

       :ui
       doom
       doom-dashboard
       (emoji +unicode)
       hl-todo
       hydra
       modeline
       nav-flash
       ophints
       (popup +defaults)
       (vc-gutter +pretty +diff-hl)
       window-select

       :editor
       evil
       file-templates
       format
       snippets

       :emacs
       dired
       electric
       ibuffer
       undo
       vc

       :term
       eshell
       vterm

       :checkers
       (syntax +childframe)
       spell

       :tools
       direnv
       editorconfig
       (eval +overlay)
       (lookup +docsets)
       (lsp +peek)
       (magit +forge)
       taskrunner

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       (cc +lsp)
       csharp
       data
       (dart +flutter +lsp)
       emacs-lisp
       (go +lsp)
       (graphql +lsp)
       (haskell +lsp)
       json
       (java +lsp)
       (javascript +lsp)
       latex
       lua
       markdown
       (nix +lsp)
       (org +pretty)
       (python +lsp +pyright +cython)
       (ruby +rails)
       (rust +lsp)
       (scheme +guile)
       (sh +fish)
       (web +css +html)
       yaml

       :email
       (mu4e +org +gmail)
       (wanderlust +gmail)

       :app
       (rss +org)

       :config
       (default +bindings +smartparens))

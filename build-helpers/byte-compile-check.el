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

;; Run with emacs --script

(let ((els))
  (dolist (path load-path)
    (dolist (el (file-expand-wildcards (expand-file-name "*.el" path)))
      (unless (or (string-suffix-p "-pkg.el" el)
                  (string-suffix-p "-theme.el" el)
                  (string-suffix-p "-autoloads.el" el)
                  (file-exists-p (concat el "c"))
                  (with-temp-buffer
                    (insert-file-contents el)
                    ;; This is wrong, but close enough for our purposes.
                    (search-forward "no-byte-compile: t" nil t)))
        (push el els))))
  (princ (string-join (seq-sort #'string< els) "\n")))

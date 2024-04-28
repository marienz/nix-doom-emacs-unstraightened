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

(require 'package)

(with-temp-buffer
  (setq default-directory (car command-line-args-left))
  (dired-mode)
  ;; Ignore dependency extraction errors because it fails for repos not
  ;; containing a "proper" package (no -pkg.el, no file with the right magic
  ;; header). These seem common enough to be not worth allowlisting.
  (let ((reqs (with-demoted-errors "Extracting dependencies: %s"
                (package-desc-reqs (package-dir-info)))))
    (princ
     (json-encode
      (mapcar #'car (seq-remove (lambda (p) (apply #'package-built-in-p p))
                                reqs))))))

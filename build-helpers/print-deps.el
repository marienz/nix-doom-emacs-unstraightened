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
(require 'lisp-mnt)

;; package-dir-info is awkward:
;; - When looking for -pkg.el, it extracts the package name from the directory.
;;   That does not work for us: our directory is a store path.
;; - When looking for .el files with package headers, it accepts the first one
;;   for which package-buffer-info returns non-nil.
;;   That does not have the same problem, but it has a different one:
;;   things like company-lean and lean-mode build from the same repo.
;;
;; So we pass the package name in explicitly and roll our own.
;;
;; Melpa is trying to phase out -pkg.el files. Since we need this for fewer
;; packages than Melpa does, try to do the same.
;;
;; And package-buffer-info requires a Version or Package-Version header not
;; everything has, so grab dependencies directly.

(let* ((directory (car command-line-args-left))
       (name (cadr command-line-args-left))
       (file (expand-file-name (format "%s.el" name) directory))
       (file (if (file-exists-p file)
                 file
               ;; HACK for x-face-e21.el (normally found through recipe)
               (car (file-expand-wildcards
                     (format "%s/*/%s.el" directory name)))))
       (reqs (lm-package-requires file))
       (desc (package-desc-from-define name "9999snapshot1" nil reqs))
       (parsed-reqs (package-desc-reqs desc))
       (filtered-reqs (cl-remove #'package-built-in-p parsed-reqs :test #'apply))
       (req-names (mapcar #'car filtered-reqs)))
  (princ (json-encode req-names)))

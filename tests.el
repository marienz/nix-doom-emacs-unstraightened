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

(defun test-minimal ()
  ;; The minimal test is a noop.
  )

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

(add-hook 'doom-after-init-hook 'test-doom)

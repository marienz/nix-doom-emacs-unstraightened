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

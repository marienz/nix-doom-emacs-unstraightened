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

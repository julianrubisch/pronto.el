;;; pronto.el --- Compilation mode for pronto

;;; Commentary:

;;; Code:
(defvar pronto-last-commit nil
  "Last commit pronto was run against.")

(defvar pronto-last-args nil
  "Last arguments pronto was run with.")

(defun pronto-run (commit)
  "Run pronto against COMMIT."
  (interactive (list (read-string "Commit: " pronto-last-commit)))
  (pronto-compile commit)
  )

(define-compilation-mode pronto-compilation-mode "Pronto Compilation"
  "Compilation mode for pronto output.")

(defun pronto-compile (commit)
  "Start a compilation against COMMIT."
  (setq pronto-last-commit commit)
  (compile (format "bundle exec pronto run -c=%s" commit) 'pronto-compilation-mode))

(defvar pronto-compilation-error-regexp-alist-alist
  '((pronto "^\\([0-9A-Za-z@_./:-]+\\.rb\\):\\([0-9]+\\)" 1 2 nil 2 1)))

(defvar pronto-compilation-error-regexp-alist
  (mapcar 'car pronto-compilation-error-regexp-alist-alist))

(provide 'pronto)
;;; pronto.el ends here

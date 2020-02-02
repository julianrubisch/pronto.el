;;; pronto.el --- Compilation mode for pronto -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Julian Rubisch

;; Author: Julian Rubisch <julian@julianrubisch.at>
;; Version: 1.0
;; Keywords: processes, tools
;; URL: https://github.com/julianrubisch/pronto.el
;; Package-Requires: ((emacs "24"))

;;; Commentary:
;; Run pronto (https://github.com/prontolabs/pronto) in a compilation mode and
;; presents errors in a browsable style.

;;; Code:
(require 'compile)
(require 'ansi-color)

(defvar pronto-compilation-mode)

(defvar pronto-last-commit nil
  "Last commit pronto was run against.")

(defvar pronto-last-args nil
  "Last arguments pronto was run with.")

(defun pronto-run (commit)
  "Run pronto against COMMIT."
  (interactive (list (read-string "Commit: " pronto-last-commit)))
  (pronto-compile commit))

(define-compilation-mode pronto-compilation-mode "Pronto Compilation"
  "Compilation mode for pronto output."
  (add-hook 'compilation-filter-hook #'pronto-colorize-compilation-buffer nil t))

(defun pronto-colorize-compilation-buffer()
  "Colorize the compilation buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))

(defun pronto-compile (commit)
  "Start a compilation against COMMIT."
  (setq pronto-last-commit commit)
  (compile (format "bundle exec pronto run -c=%s" commit) #'pronto-compilation-mode))

(defvar pronto-compilation-error-regexp-alist-alist
  '((pronto "^\\([0-9A-Za-z@_./:-]+\\.rb\\):\\([0-9]+\\)" 1 2 nil 2 1)))

(defvar pronto-compilation-error-regexp-alist
  (mapcar 'car pronto-compilation-error-regexp-alist-alist))

(provide 'pronto)
;;; pronto.el ends here

;;;; -*- emacs-lisp -*-

(defvar forth-mode-map)

(defvar forth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "<" table)
    (modify-syntax-entry ?\) ">" table)
    (modify-syntax-entry ?\: "(" table)
    (modify-syntax-entry ?\; ")" table)
    (modify-syntax-entry ?[ "(" table)
    (modify-syntax-entry ?] ")" table)
    (modify-syntax-entry ?\? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?, "_" table)
    table))

(defvar forth-mode-hooks)

;;;### autoload
(defun forth-mode ()
  "Major mode for editing Forth files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table forth-mode-syntax-table)
  ;; (make-local-variable ...)
  (setq major-mode 'forth-mode
	mode-name "Forth"
	;; font-lock-defaults
	indent-line-function #'forth-indent
	comment-indent-function #'forth-indent-comment
	comment-start-skip "\\((\\*?\\|\\\\\\) *"
	comment-start "("
	comment-end ")")
  (run-mode-hooks 'forth-mode-hooks))

(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\)\\'" . forth-mode))

;;; : ; does> variable constant value
;;; if else then  do loop begin while repeat again until  postpone

(defun forth-forward-sexp ())
(defun forth-backward-sexp ())
(defun forth-kill-sexp ())
(defun forth-beginning-of-defun ())
(defun forth-end-of-defun ())

(provide 'forth-mode)

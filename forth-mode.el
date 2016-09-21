;;; forth-mode.el --- Programming language mode for Forth
;;; Copyright 2014 Lars Brinkhoff

;; Author: Lars Brinkhoff <lars@nocrew.org>
;; Keywords: languages forth
;; URL: http://github.com/larsbrinkhoff/forth-mode

;;; Commentary:
;; Programming language mode for Forth

;;; Code:

(load-library "forth-block-mode.el")

(defvar forth-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key (kbd "C-x C-e") #'forth-eval-last-sexp)
    ;; (define-key (kbd "C-M-x") #'forth-eval-defun)
    ;; (define-key (kbd "C-c C-r") #'forth-eval-region)
    ;; eval-buffer
    ;; compile-region
    ;; (define-key (kbd "C-x C-e") #'forth-eval-last-sexp)
    ;; (define-key (kbd "C-M-x") #'forth-eval-defun)
    ;; (define-key (kbd "C-c C-r") #'forth-eval-region)
    (define-key map (kbd "C-c C-l") 'forth-load-file)
    ;; (define-key (kbd "C-c :") #'forth-eval-expression)
    ;; (define-key (kbd "C-c C-c") #'forth-compile-defun)
    ;; (define-key (kbd "C-c C-k") #'forth-compile-and-load-file)
    ;; (define-key (kbd "C-c M-k") #'forth-compile-file)
    ;; (define-key (kbd "C-x `") #'forth-next-error)
    ;; (define-key (kbd "M-n") #'forth-next-note)
    ;; (define-key (kbd "M-p") #'forth-previous-note)
    ;; (define-key (kbd "M-TAB") #'forth-complete-symbol)
    ;; (define-key (kbd "M-.") #'forth-edit-definition)
    ;; (define-key (kbd "C-c M-d") #'forth-disassemble-symbol)
    ;; (define-key (kbd "C-c M-d") #'forth-disassemble-symbol)
    ;; (define-key (kbd "C-c C-z") #'forth-switch-to-output-buffer)
    map))

(defvar forth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "<1" table)
    (modify-syntax-entry ?\) ">4" table)
    (modify-syntax-entry ?* "_23n" table)
    (modify-syntax-entry ?\{ "<" table)
    (modify-syntax-entry ?\} ">" table)
    (modify-syntax-entry ?\: "(" table)
    (modify-syntax-entry ?\; ")" table)
    (modify-syntax-entry ?\[ "_" table)
    (modify-syntax-entry ?\] "_" table)
    (modify-syntax-entry ?\? "_" table)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?< "_" table)
    (modify-syntax-entry ?> "_" table)
    (modify-syntax-entry ?. "_" table)
    (modify-syntax-entry ?, "_" table)
    (modify-syntax-entry ?' "_" table)
    (modify-syntax-entry ?\" "\"" table)
    table))

(defvar forth-mode-hook)

(defvar forth-font-lock-keywords
  '((forth-match-definition 3 font-lock-function-name-face)))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

;;;### autoload
(define-derived-mode forth-mode prog-mode "Forth"
		     "Major mode for editing Forth files."
		     :syntax-table forth-mode-syntax-table
  (if (forth-block-p)
      (forth-block-mode))
  (setq font-lock-defaults '(forth-font-lock-keywords))
  (setq ;; font-lock-defaults
	indent-line-function #'forth-indent
	comment-indent-function #'forth-indent-comment
	comment-start-skip "\\((\\*?\\|\\\\\\) *"
	comment-start "("
	comment-end ")"
	imenu-generic-expression
	'(("Words"
	   "^\\s-*\\(:\\|code\\|defer\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
	  ("Variables"
	   "^\\s-*2?\\(variable\\|create\\|value\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
	  ("Constants"
	   "\\s-2?constant\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 1))))

(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\)\\'" . forth-mode))

;;; : ; does> variable constant value
;;; if else then  do loop begin while repeat again until  postpone

(defun forth-match-definition (limit)
  (search-forward-regexp "\\(^\\|\\s-\\)\\(\\S-*:\\|code\\|defer\\|2?variable\\|create\\|2?value\\|2?constant\\)\\s-+\\([[:graph:]]+\\)"
			 limit t))

(defun forth-forward-sexp ())
(defun forth-backward-sexp ())
(defun forth-kill-sexp ())
(defun forth-beginning-of-defun ())
(defun forth-end-of-defun ())

(defun forth-load-file (file)
  (interactive (list (buffer-file-name (current-buffer))))
  (forth-interaction-send (concat "include " file)))

(provide 'forth-mode)
;;; forth-mode.el ends here

;;; forth-mode.el --- Programming language mode for Forth
;;; Copyright 2014 Lars Brinkhoff

;; Author: Lars Brinkhoff <lars@nocrew.org>
;; Keywords: languages forth
;; URL: http://github.com/larsbrinkhoff/forth-mode
;; Version: 0.1

;;; Commentary:
;; Programming language mode for Forth

;;; Code:

(defvar forth-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'forth-eval-region)
    (define-key map (kbd "C-c C-l") 'forth-load-file)
    ;; (define-key map (kbd "C-M-x") #'forth-eval-defun)
    ;; (define-key map (kbd "C-c C-c") 'eval-buffer)
    ;; (define-key map (kbd "C-x C-e") #'forth-eval-last-sexp)
    ;; (define-key map (kbd "C-M-x") #'forth-eval-defun)
    ;; (define-key map (kbd "C-c :") #'forth-eval-expression)
    ;; (define-key map (kbd "C-x `") #'forth-next-error)
    ;; (define-key map (kbd "M-n") #'forth-next-note)
    ;; (define-key map (kbd "M-p") #'forth-previous-note)
    ;; (define-key map (kbd "M-TAB") #'forth-complete-symbol)
    ;; (define-key map (kbd "M-.") #'forth-find-definition)
    ;; (define-key map (kbd "C-c C-s") #'forth-see)
    ;; (define-key map (kbd "C-c C-z") #'forth-switch-to-output-buffer)
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

(defun forth-symbol-start ()
  (save-excursion
    (re-search-backward "[^[:graph:]]")
    (1+ (point))))

(defun forth-symbol-end ()
  (save-excursion
    (re-search-forward "[^[:graph:]]")
    (1- (point))))

(defun forth-expand-symbol ()
  (let ((list (forth-words)))
    (dolist (index (imenu--make-index-alist t))
      (when (listp (rest index))
	(dolist (def (rest index))
	  (push (car def) list))))
    (list (forth-symbol-start) (forth-symbol-end) list)))

(defun forth-block-p ()
  "Guess whether the current buffer is a Forth block file."
  (and (> (point-max) 1)
       (eq (logand (point-max) 1023) 1)
       (save-excursion
	 (forth-beginning)
	 (not (search-forward "\n" 1024 t)))))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode forth-mode prog-mode "Forth"
		     "Major mode for editing Forth files."
		     :syntax-table forth-mode-syntax-table
  (if (forth-block-p)
      (forth-block-mode))
  (setq font-lock-defaults '(forth-font-lock-keywords))
  (setq-local completion-at-point-functions '(forth-expand-symbol))
  (setq ;; font-lock-defaults
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

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(f\\|fs\\|fth\\|4th\\)\\'" . forth-mode))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (lib &rest forms)
    `(eval-after-load ,lib '(progn ,@forms))))

(eval-after-load "speedbar"
  '(progn
    (speedbar-add-supported-extension ".f")
    (speedbar-add-supported-extension ".fs")
    (speedbar-add-supported-extension ".fth")
    (speedbar-add-supported-extension ".4th")))

;;; : ; does> variable constant value
;;; if else then  do loop begin while repeat again until  postpone

(defun forth-match-definition (limit)
  (search-forward-regexp "\\(^\\|\\s-\\)\\(\\S-*:\\|code\\|defer\\|2?variable\\|create\\|2?value\\|2?constant\\)\\s-+\\([[:graph:]]+\\)"
			 limit t))

(defun forth-load-file (file)
  (interactive (list (buffer-file-name (current-buffer))))
  (forth-interaction-send "include " file))

(defun forth-beginning ()
  (goto-char (point-min)))

(provide 'forth-mode)
;;; forth-mode.el ends here

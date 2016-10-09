;;; forth-mode.el --- Programming language mode for Forth
;;; Copyright 2014 Lars Brinkhoff

;; Author: Lars Brinkhoff <lars@nocrew.org>
;; Keywords: languages forth
;; URL: http://github.com/larsbrinkhoff/forth-mode
;; Version: 0.1

;;; Commentary:
;; Programming language mode for Forth

;;; Code:

(require 'cl)
(require 'forth-smie)

(defvar forth-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") 'forth-eval-region)
    (define-key map (kbd "C-c C-l") 'forth-load-file)
    (define-key map (kbd "C-c C-s") 'forth-see)
    (define-key map (kbd "C-M-x") 'forth-eval-defun)
    (define-key map (kbd "C-c C-k") 'forth-kill)
    (define-key map (kbd "C-c C-e") 'forth-eval-last-expression)
    (define-key map (kbd "C-x M-e") 'forth-eval-last-expression-display-output)
    (define-key map (kbd "C-c C-z") 'forth-switch-to-output-buffer)
    (define-key map (kbd "C-c :") 'forth-eval)
    ;; (define-key map (kbd "C-c C-c") 'eval-buffer)
    ;; (define-key map (kbd "C-x `") #'forth-next-error)
    ;; (define-key map (kbd "M-n") #'forth-next-note)
    ;; (define-key map (kbd "M-p") #'forth-previous-note)
    ;; (define-key map (kbd "M-.") #'forth-find-definition)
    map))

(defvar forth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\( "!" table)
    (modify-syntax-entry ?\) "_" table)
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

(defun forth-word-at-point ()
  (buffer-substring (forth-symbol-start) (forth-symbol-end)))

(defun forth--ppss-in-comment-p (pos)
  (not (null (elt (syntax-ppss pos) 4))))

(defun forth--syntax-propertize (start end)
  (save-excursion
    (goto-char start)
    ;; Fix some cases of comment syntax
    (while (re-search-forward "(\\|\\\\" end t)
      (when (and (forth--ppss-in-comment-p (point))
		 (not (forth--ppss-in-comment-p (1- (point)))))
	(cond ((save-excursion
		 (goto-char (1- (point)))
		 (not (looking-at "\\([ \n]\\|\\\`\\)\\((\\|\\\\\\)[ \n]")))
	       (put-text-property (point) (forth-symbol-end)
				  'syntax-table (string-to-syntax "_")))
	      ((and (looking-at "(")
		    (re-search-forward ")" nil t))
	       (put-text-property (point) (1+ (point))
				  'syntax-table (string-to-syntax "!")))))
      (forward-char))))

(defun forth-expand-symbol ()
  (let ((list (forth-words)))
    (when (fboundp 'imenu--make-index-alist)
      (dolist (index (imenu--make-index-alist t))
	(when (listp (rest index))
	  (dolist (def (rest index))
	    (push (car def) list)))))
    (list (forth-symbol-start) (forth-symbol-end) list)))

(defun forth-block-with-newlines-p ()
  (save-excursion
    (forth-beginning)
    (let ((result t))
      (dotimes (i 16)
	(goto-char (* 64 (1+ i)))
	(unless (looking-at "\n")
	  (setq result nil)))
      result)))

(defun forth-block-without-newlines-p ()
  (save-excursion
    (forth-beginning)
    (not (search-forward "\n" 1024 t))))

(defun forth-block-p ()
  "Guess whether the current buffer is a Forth block file."
  (and (> (point-max) 1)
       (eq (logand (point-max) 1023) 1)
       (or (forth-block-with-newlines-p)
	   (forth-block-without-newlines-p))))

(unless (fboundp 'prog-mode)
  (defalias 'prog-mode 'fundamental-mode))

(unless (fboundp 'setq-local)
  (defmacro setq-local (var val)
    `(set (make-local-variable ',var) ,val)))

;;;###autoload
(define-derived-mode forth-mode prog-mode "Forth"
		     "Major mode for editing Forth files."
		     :syntax-table forth-mode-syntax-table
  (if (forth-block-p)
      (forth-block-mode))
  (setq font-lock-defaults '(forth-font-lock-keywords))
  (setq-local completion-at-point-functions '(forth-expand-symbol))
  (setq-local syntax-propertize-function #'forth--syntax-propertize)
  (setq-local parse-sexp-lookup-properties t)
  (forth-smie-setup)
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

(defun forth-beginning ()
  (goto-char (point-min)))

(provide 'forth-mode)
;;; forth-mode.el ends here

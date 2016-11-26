;;; forth-mode.el --- Programming language mode for Forth
;;; Copyright 2014 Lars Brinkhoff

;; Author: Lars Brinkhoff <lars@nocrew.org>
;; Keywords: languages forth
;; URL: http://github.com/larsbrinkhoff/forth-mode
;; Version: 0.1

;;; Commentary:
;; Programming language mode for Forth

;;; Code:

(eval-when-compile (byte-compile-disable-warning 'cl-functions))
(require 'cl)
(require 'forth-syntax)
(require 'forth-smie)
(require 'forth-spec)

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
    (define-key map (kbd "C-c C-d 1") 'forth-spec-lookup-1994)
    (define-key map (kbd "C-c C-d 2") 'forth-spec-lookup-2012)
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
    (modify-syntax-entry ?\( "<1b" table)
    (modify-syntax-entry ?\) ">4b" table)
    (modify-syntax-entry ?* "_23n" table)
    (modify-syntax-entry ?\{ "_" table)
    (modify-syntax-entry ?\} "_" table)
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

(defun forth-symbol-start ()
  (save-excursion
    (skip-chars-backward forth-syntax-non-whitespace)
    (point)))

(defun forth-symbol-end ()
  (save-excursion
    (skip-chars-forward forth-syntax-non-whitespace)
    (point)))

(defun forth-word-at-point ()
  (buffer-substring (forth-symbol-start) (forth-symbol-end)))

(defun forth-expand-symbol ()
  (let ((list (forth-words)))
    (when (fboundp 'imenu--make-index-alist)
      (dolist (index (imenu--make-index-alist t))
	(when (listp (rest index))
	  (dolist (def (rest index))
	    (push (car def) list)))))
    (list (forth-symbol-start) (forth-symbol-end)
	  ;; FIXME: this should use `completion-table-case-fold' or
	  ;; closures but neither is available in Emacs23.
	  `(lambda (string pred action)
	     (let ((completion-ignore-case t))
	       (complete-with-action action ',list string pred))))))

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

;; This just calls the standard `fill-paragraph' with adjusted
;; paramaters.
(defun forth-fill-paragraph (&rest args)
  (let ((fill-paragraph-function nil)
	(fill-paragraph-handle-comment t)
	(comment-start "\\ ")
	(comment-end ""))
    (apply #'fill-paragraph args)))

(defun forth-comment-region (&rest args)
  (let ((comment-start "\\ ")
	(comment-end ""))
    (apply #'comment-region-default args)))

(defun forth-beginning-of-defun (arg)
  (and (re-search-backward "^\\s *: \\_<" nil t (or arg 1))
       (beginning-of-line)))

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
  (setq font-lock-defaults '(nil))
  (setq-local completion-at-point-functions '(forth-expand-symbol))
  (when (boundp 'syntax-propertize-function)
    (setq-local syntax-propertize-function #'forth-syntax-propertize))
  (setq-local parse-sexp-lookup-properties t)
  (forth-smie-setup)
  (setq-local fill-paragraph-function #'forth-fill-paragraph)
  (setq-local beginning-of-defun-function #'forth-beginning-of-defun)
  (setq-local comment-start-skip "[(\\][ \t*]+")
  (setq-local comment-start "( ")
  (setq-local comment-end " )")
  (setq-local comment-region-function #'forth-comment-region)
  (setq imenu-generic-expression
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

(with-eval-after-load "speedbar"
  (when (fboundp 'speedbar-add-supported-extension)
    (speedbar-add-supported-extension ".f")
    (speedbar-add-supported-extension ".fs")
    (speedbar-add-supported-extension ".fth")
    (speedbar-add-supported-extension ".4th")))

(defun forth-beginning ()
  (goto-char (point-min)))

(provide 'forth-mode)
;;; forth-mode.el ends here

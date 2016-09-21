(require 'forth-mode)

(defvar forth-stack-comments (make-hash-table :test 'equal))

(defun forth-parse-colon-definition ()
  (forward-char)
  (re-search-forward "[[:graph:]]")
  (backward-char)
  (let ((start (point)))
    (re-search-forward "[^[:graph:]]")
    (let ((name (buffer-substring start (1- (point)))))
      (when (looking-at "(")
	(forward-char 2)
	(let ((start (point)))
	  (search-forward ")")
	  (setf (gethash name forth-stack-comments)
		(buffer-substring start (1- (point)))))))))

(defun forth-parse-definition ()
  (cond ((looking-at ":") (forth-parse-colon-definition))
	((looking-at "create") t)
	((looking-at "variable") t)
	((looking-at "2variable") t)
	((looking-at "defer") t)
	((looking-at "code") t)))

(defun forth-parse-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (save-excursion
    (forth-beginning)
    (end-of-defun)
    (beginning-of-defun)
    (while t
      (forth-parse-definition)
      (end-of-defun)
      (end-of-defun)
      (beginning-of-defun))))

(defun forth-word-at-point ()
  (if (looking-at "[^[:graph:]]")
      nil
      (save-excursion
	(re-search-backward "[^[:graph:]]")
	(forward-char)
	(let ((start (point)))
	  (re-search-forward "[^[:graph:]]")
	  (buffer-substring start (1- (point)))))))

(defun forth-stack-comment ()
  (let ((word (forth-word-at-point)))
    (when word
      (let ((stack-comment (gethash word forth-stack-comments)))
	(when stack-comment
	  (message "%s" stack-comment))))))

(defun forth-stack-comments-mode ()
  (interactive)
  (add-hook 'post-command-hook 'forth-stack-comment nil t))

(provide 'forth-parse)

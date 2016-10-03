(require 'comint)
(require 'forth-mode)

(defvar forth-interaction-buffer nil)
(defvar forth-interaction-callback nil)
(defvar forth-words-cache nil)

(defvar forth-interaction-mode-map
  (let ((map (copy-keymap forth-mode-map)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-r") 'forth-restart)
    map)
  "Keymap for Forth interaction.")

(define-derived-mode forth-interaction-mode comint-mode "Forth Interaction"
  "Major mode for interacting with Forth."
  :syntax-table forth-mode-syntax-table
  (use-local-map forth-interaction-mode-map))

(defun forth-interaction-preoutput-filter (text)
  (if forth-interaction-callback
      (funcall forth-interaction-callback text)
      text))

;;;###autoload
(defun forth-kill (&optional buffer)
  (interactive)
  (setq buffer (or buffer forth-interaction-buffer))
  (when (get-buffer-process buffer)
    (set-process-query-on-exit-flag (get-buffer-process buffer) nil))
  (kill-buffer buffer)
  (setq forth-interaction-buffer nil))

(defun forth-interaction-sentinel (proc arg)
  (message "Forth: %s" arg)
  (forth-kill (process-buffer proc)))

(defvar forth-executable nil)

(defvar run-forth-hooks)

;;;###autoload
(defun run-forth ()
  "Start an interactive forth session."
  (interactive)
  (unless forth-executable
    (setq forth-executable
	  (read-string "Forth executable: ")))
  (let ((buffer (get-buffer-create "*forth*")))
    (pop-to-buffer buffer)
    (unless (comint-check-proc buffer)
      (run-hooks 'run-forth-hooks)
      (make-comint-in-buffer "forth" buffer forth-executable)
      (set-process-sentinel (get-buffer-process buffer)
			    'forth-interaction-sentinel)
      (forth-interaction-mode)
      (add-hook 'comint-preoutput-filter-functions
		'forth-interaction-preoutput-filter nil t)
      (setq forth-interaction-buffer buffer))))
      
;;;###autoload
(defun forth-restart ()
  (interactive)
  (forth-kill)
  (run-forth))

(defun forth-ensure ()
  (unless forth-interaction-buffer
    (run-forth))
  (get-buffer-process forth-interaction-buffer))

(defun forth-scrub (string)
  "Remove terminal escape sequences from STRING."
  (let ((n 0))
    (while (setq n (string-match "[?[0-9;]*[a-z]" string n))
      (message "n = %d" n)
      (setq string (replace-match "" t t string))))
  (setq string (replace-regexp-in-string "\\`[[:space:]\n]*" "" string))
  (setq string (replace-regexp-in-string "[[:space:]\n]*\\'" "" string))
  (setq string (replace-regexp-in-string "ok\\'" "" string))
  (setq string (replace-regexp-in-string "[[:space:]\n]*\\'" "" string)))

;;;###autoload
(defun forth-interaction-send (&rest strings)
  (let* ((proc (forth-ensure))
	 (forth-result nil)
	 (forth-interaction-callback (lambda (x)
				       (setq forth-result (concat forth-result x))
				       ""))
	 (end-time (+ (float-time) .4)))
    (dolist (s strings)
      (comint-send-string proc s))
    (comint-send-string proc "\n")
    (while (< (float-time) end-time)
      (accept-process-output proc 0.1))
    (setq forth-words-cache nil)
    (forth-scrub forth-result)))

;;;###autoload
(defun forth-words ()
  (when forth-interaction-buffer
    (or forth-words-cache
	(setq forth-words-cache
	      (split-string (forth-interaction-send "words"))))))

;;;###autoload
(defun forth-eval (string)
  (interactive "sForth expression: ")
  (message "%s" (forth-interaction-send string)))

;;;###autoload
(defun forth-eval-region (start end)
  (interactive "r")
  (forth-eval (buffer-substring start end)))

;;;###autoload
(defun forth-eval-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (forth-eval-region (point) (mark))))

;;;###autoload
(defun forth-load-file (file)
  (interactive (list (buffer-file-name (current-buffer))))
  (forth-interaction-send "include " file))

;;;###autoload
(defun forth-see (word)
  (interactive (list (forth-word-at-point)))
  (let ((buffer (get-buffer-create "*see*")))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (forth-interaction-send "see " word)))
    (special-mode)))

;;;###autoload
(defun forth-switch-to-output-buffer ()
  (interactive)
  (if forth-interaction-buffer
      (switch-to-buffer forth-interaction-buffer)
      (message "Forth not started.")))

;;;###autoload
(defun forth-eval-last-expression ()
  (interactive)
  (save-excursion
    (backward-sexp)
    (let ((start (point)))
      (forward-sexp)
      (forth-eval-region start (point)))))

;;;###autoload
(defun forth-eval-last-expression-display-output ()
  (interactive)
  (if forth-interaction-buffer
      (save-excursion
	(backward-sexp)
	(let ((start (point)))
	  (forward-sexp)
	  (let ((string (buffer-substring start (point))))
	    (forth-switch-to-output-buffer)
	    (insert (forth-interaction-send string)))))
      (message "Forth not started.")))

(provide 'forth-interaction-mode)

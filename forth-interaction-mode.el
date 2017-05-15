(eval-when-compile (byte-compile-disable-warning 'cl-functions))

(require 'comint)
(require 'forth-mode)

(defvar forth-interaction-buffer nil)
(defvar forth-interaction-source-buffer nil)
(defvar forth-interaction-callback nil)
(defvar forth-words-cache nil)
(defvar forth-implementation nil)
(defvar forth-banner "")
(defvar forth-backend-dir
  (concat (file-name-directory load-file-name) "backend"))

(defvar forth-implementation-matches
  '(("Gforth" . gforth)
    ("SP-FORTH" . spforth)
    ("PForth" . pforth)
    ("VFX Forth" . vfxforth)
    ("SwiftForth" . swiftforth)
    ("lbForth" . lbforth)))

(defvar forth-interaction-mode-map
  (let ((map (copy-keymap forth-mode-map)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-r") 'forth-restart)
    (define-key map (kbd "C-c C-z") 'forth-switch-to-source-buffer)
    map)
  "Keymap for Forth interaction.")

(define-derived-mode forth-interaction-mode comint-mode "Forth Interaction"
  "Major mode for interacting with Forth."
  :syntax-table forth-mode-syntax-table
  (use-local-map forth-interaction-mode-map))

(defvar forth-interaction-init-backend-hook '())

(defun forth-interaction-preoutput-filter (text)
  (unless forth-implementation
    (setq forth-banner (concat forth-banner text))
    (dolist (x forth-implementation-matches)
      (when (string-match (car x) forth-banner)
	(setq forth-implementation (cdr x))
	(let ((load-path (cons forth-backend-dir load-path)))
	  (require forth-implementation))
	(run-hook-with-args 'forth-interaction-init-backend-hook
			    forth-implementation
			    (get-buffer-process (current-buffer))))))
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
  ;;FIXME: Can't do this because it calls process-mark, which
  ;; errors out in killed processes.  Still, would be nice to see
  ;; something in the *forth* buffer.
  ;;(comint-output-filter proc (format "\nForth: %s\n" arg))
  )

(defvar forth-executable nil)

(defvar run-forth-hooks)

;;;###autoload
(defun run-forth ()
  "Start an interactive forth session."
  (interactive)
  (setq forth-implementation nil)
  (setq forth-banner "")
  (unless forth-executable
    (setq forth-executable
	  (read-string "Forth executable: ")))
  (let ((buffer (get-buffer-create "*forth*")))
    (pop-to-buffer buffer)
    (unless (comint-check-proc buffer)
      (run-hooks 'run-forth-hooks)
      (make-comint-in-buffer "forth" buffer forth-executable)
      (set-process-window-size (get-buffer-process buffer)
			       (window-height) (window-width))
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
  (unless (buffer-live-p forth-interaction-buffer)
    (run-forth))
  (get-buffer-process forth-interaction-buffer))

(defun forth-scrub (string &optional keep-ok)
  "Remove terminal escape sequences from STRING."
  (let ((n 0))
    (while (setq n (string-match "[?[0-9;]*[a-z]" string n))
      (setq string (replace-match "" t t string))))
  (setq string (replace-regexp-in-string "\\`[[:space:]\n]*" "" string))
  (setq string (replace-regexp-in-string "[[:space:]\n]*\\'" "" string))
  (if keep-ok
      string
    (setq string (replace-regexp-in-string "ok\\'" "" string))
    (setq string (replace-regexp-in-string "[[:space:]\n]*\\'" "" string))))

(defun forth-interaction-send-raw-result (&rest strings)
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
    forth-result))

;;;###autoload
(defun forth-interaction-send (&rest strings)
  (forth-scrub (apply #'forth-interaction-send-raw-result strings)))

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
  (let ((result (forth-interaction-send-raw-result "include " file)))
    (setq result (forth-scrub result t))
    (if (< (count ?\n result) 2)
	(message "%s" result)
      (pop-to-buffer forth-interaction-buffer))
    (comint-output-filter (get-buffer-process forth-interaction-buffer)
			  (concat result "\n"))))

;;;###autoload
(defun forth-see (word)
  (interactive (list (forth-word-at-point)))
  (let ((buffer (get-buffer-create "*see*")))
    (pop-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (forth-interaction-send "see " word)))
    (special-mode)))

(defun forth-switch-to-buffer (buffer)
  ;; If buffer is visible, switch to that window.  Otherwise, display
  ;; buffer in current window.
  (select-window (display-buffer buffer
				 '((display-buffer-reuse-window
				    display-buffer-same-window)))))

;;;###autoload
(defun forth-switch-to-output-buffer ()
  (interactive)
  (if forth-interaction-buffer
      (progn
	(setq forth-interaction-source-buffer (current-buffer))
	(forth-switch-to-buffer forth-interaction-buffer))
      (message "Forth not started.")))

;;;###autoload
(defun forth-switch-to-source-buffer ()
  (interactive)
  (if forth-interaction-source-buffer
      (forth-switch-to-buffer forth-interaction-source-buffer)
    (message "Don't know which buffer to switch to.")))

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

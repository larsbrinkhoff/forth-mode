(require 'comint)

(defvar forth-interaction-buffer nil)
(defvar forth-interaction-callback nil)

(defvar forth-interaction-mode-map
  (let ((map (copy-keymap forth-mode-map)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-k") 'forth-kill)
    (define-key map (kbd "C-c C-r") 'forth-restart)
    map)
  "Keymap for Forth interaction.")

(define-derived-mode forth-interaction-mode comint-mode "Forth Interaction"
  "Major mode for interacting with Forth."
  :syntax-table forth-mode-syntax-table
  (use-local-map forth-interaction-mode-map))

(defun forth-interaction-preoutput-filter (text)
  (if forth-interaction-callback
      (prog1 (when forth-interaction-callback
	       (funcall forth-interaction-callback text))
	(setq forth-interaction-callback nil))
      text))

(defun forth-kill (&optional buffer)
  (interactive)
  (setq buffer (or buffer (current-buffer)))
  (set-process-query-on-exit-flag (get-buffer-process buffer) nil)
  (kill-buffer buffer)
  (setq forth-interaction-buffer nil))

(defun forth-interaction-sentinel (proc arg)
  (message "Forth: %s" arg)
  (forth-kill (process-buffer proc)))

(defvar forth-executable "gforth")

;;;### autoload
(defun forth ()
  "Start an interactive forth session."
  (interactive)
  (let ((buffer (get-buffer-create "*forth*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "forth" buffer forth-executable)
      (set-process-sentinel (get-buffer-process buffer)
			    'forth-interaction-sentinel)
      (forth-interaction-mode)
      (add-hook 'comint-preoutput-filter-functions
		'forth-interaction-preoutput-filter nil t)
      (setq forth-interaction-buffer buffer))))
      
;;;### autoload
(defun forth-restart ()
  (interactive)
  (forth-kill)
  (forth))

(defun ensure-forth ()
  (unless forth-interaction-buffer
    (forth))
  (get-buffer-process forth-interaction-buffer))

;;;### autoload
(defun forth-interaction-send (string &optional callback)
  (let ((proc (ensure-forth)))
    (setq forth-interaction-callback callback)
    (comint-send-string proc string)
    (comint-send-string proc "\n")))

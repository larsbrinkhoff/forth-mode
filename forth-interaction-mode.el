(require 'comint)

(defvar forth-interaction-mode-map
  (let ((map (copy-keymap forth-mode-map)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "C-c C-k") 'forth-interaction-kill)
    map)
  "Keymap for Forth interaction.")

(define-derived-mode forth-interaction-mode comint-mode "Forth Interaction"
  "Major mode for interacting with Forth."
  :syntax-table forth-mode-syntax-table
  (use-local-map forth-interaction-mode-map))

(defun forth-interaction-kill (&optional buffer)
  (interactive)
  (kill-buffer (or buffer (current-buffer))))

(defun forth-interaction-sentinel (proc arg)
  (message "Forth: %s" arg)
  (forth-interaction-kill (process-buffer proc)))

;;;### autoload
(defun forth ()
  "Start an interactive forth session."
  (interactive)
  (let ((buffer (get-buffer-create "*forth*")))
    (pop-to-buffer-same-window buffer)
    (unless (comint-check-proc buffer)
      (make-comint-in-buffer "forth" buffer "forth")
      (set-process-sentinel (get-buffer-process buffer)
			    'forth-interaction-sentinel)
      (forth-interaction-mode))))
      

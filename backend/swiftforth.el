(require 'forth-interaction-mode)

(set-process-coding-system (get-buffer-process forth-interaction-buffer)
			   'raw-text-dos 'raw-text-dos)
(save-excursion
  (with-current-buffer forth-interaction-buffer
    (goto-char (point-max))
    (insert "\n")))

(forth-interaction-send (concat "include " forth-backend-dir "/swiftforth.fth"))

(provide 'swiftforth)

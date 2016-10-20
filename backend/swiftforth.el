(require 'forth-interaction-mode)

(defun forth-swiftforth-init (backend-type process)
  (when (eq backend-type 'swiftforth)
    (set-process-coding-system process 'raw-text-dos 'raw-text-dos)
    (forth-interaction-send (concat "include " forth-backend-dir
				    "/swiftforth.fth"))))

(add-hook 'forth-interaction-init-backend-hook #'forth-swiftforth-init)

(provide 'swiftforth)

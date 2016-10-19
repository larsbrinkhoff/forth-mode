(require 'forth-interaction-mode)

(defun forth-gforth-init (backend-type process)
  (when (eq backend-type 'gforth)
    (forth-interaction-send "' drop is Attr!")))

(add-hook 'forth-interaction-init-backend-hook #'forth-gforth-init)

(provide 'gforth)

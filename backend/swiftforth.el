(require 'forth-interaction-mode)

(forth-interaction-send (concat "include " forth-backend-dir "/swiftforth.fth"))

(provide 'swiftforth)

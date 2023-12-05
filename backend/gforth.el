;;                                               -*- lexical-binding:t -*-

(require 'forth-interaction-mode)

(defun forth-gforth-init (backend-type process)
  (when (eq backend-type 'gforth)
    (forth-interaction-send "' drop is Attr!")))

(add-hook 'forth-interaction-init-backend-hook #'forth-gforth-init)


;; Setup code forth-term.el

(defun forth-gforth--init-term-file ()
  (file-name-concat forth-backend-dir "gforth.fth"))

(defun forth-gforth--init-term-2 (proc)
  (let ((str (format "s\" %s\" included\n" (forth-gforth--init-term-file))))
    (term-send-string proc str))
  (let* ((ack nil)
	 (term-command-function
	  (lambda (str)
	    (message "%s" str)
	    (setq ack t))))
    (while (not ack)
      (accept-process-output proc 1))))

(defun forth-gforth--init-term (impl proc)
  (when (eq impl 'gforth)
    (forth-gforth--init-term-2 proc)))

(add-hook 'forth-term-init-backend-hook #'forth-gforth--init-term)

(provide 'gforth)

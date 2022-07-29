;; Users can call (forth-setup-eglot) to tell eglot where to find
;; folly.sh.  M-x eglot should then start the server.
;;
;; With (add-hook 'forth-mode-hook 'eglot-ensure) eglot should start
;; the server automatically in every forth-mode buffer.
;;
;; For debugging, it's often more useful to start folly in a shell
;; with:
;;
;;    shell> folly.sh --port 4567
;;
;; and then connect with:
;;
;;    M-u M-x eglot localhost:4567.
;;

(defun forth--folly-program ()
  (expand-file-name "fools/folly.sh"
		    (file-name-directory (locate-library "forth-mode"))))

(defun forth-setup-eglot ()
  (let ((folly (forth--folly-program)))
    (add-to-list 'eglot-server-programs `(forth-mode . (,folly)))))

(provide 'forth-eglot)

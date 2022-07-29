;;; -*- lexical-binding:t -*-

(require 'forth-mode)
(require 'eglot)
(require 'forth-eglot)

;; copied from eglot-tests.el
(defun forth--eglot-tests-connect (&optional timeout)
  (let* ((timeout (or timeout 10))
         (eglot-sync-connect t)
         (eglot-connect-timeout timeout))
    (apply #'eglot--connect (eglot--guess-contact))))

(defun forth--with-eglot (fun)
  (let ((eglot-server-programs '())
	(server nil))
    (forth-setup-eglot)
    (find-file "fools/main.fth")
    (should (not (eglot-managed-p)))
    (unwind-protect (save-excursion
		      (let ((eglot-server-initialized-hook
			     (lambda (s)
			       (should (not server))
			       (setq server s))))
			(forth--eglot-tests-connect))
		      (should (eglot-managed-p))
		      (funcall fun))
      ;; workaround for apparent bug in eglot-shutdown
      (condition-case e
	  (eglot-shutdown server :timeout 5)
	(error
	 (message "eglot-shutdown failed: %s" e)
	 (should (not (jsonrpc-running-p server)))))
      (should (not (eglot-managed-p))))))

(ert-deftest forth-start-eglot ()
  (forth--with-eglot (lambda () )))

(ert-deftest forth-eglot-find-definition ()
  (forth--with-eglot
   (lambda ()
     (search-forward "jsonrpc-process-requests")
     (backward-char 3)
     (let* ((backend (xref-find-backend))
	    (id (xref-backend-identifier-at-point backend))
	    (defs (xref-backend-definitions backend id))
	    (files (mapcar (lambda (x)
			     (let ((loc (xref-item-location x)))
			       (and (xref-file-location-p loc)
				    (file-name-nondirectory
				     (xref-file-location-file loc)))))
			   defs)))
       (should (member "jsonrpc.fth" files))))))

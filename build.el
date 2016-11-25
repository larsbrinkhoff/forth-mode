(princ (emacs-version))
(setq forth-executable (getenv "FORTH"))

(require 'autoload)
(let ((generated-autoload-file (concat default-directory "autoloads.el")))
  (update-directory-autoloads "." "./backend"))
(load-file "autoloads.el")
(add-to-list 'load-path default-directory)

(if (locate-library "ert")
    (require 'ert)
  (defmacro ert-deftest (name args &rest body)
    `(progn (message "Testing: %s" ',name) ,@body))
  (defun ert-run-tests-batch-and-exit (&optional x)
    (kill-emacs 0))
  (defun should (arg)
    (unless arg
      (kill-emacs 1)))
  (defun should-not (arg)
    (should (not arg))))

(ert-deftest compile-package ()
  "Compile package."
  (should-not (string-match "failed" (byte-recompile-directory "." 0))))

(load-file "test/tests.el")

;;; Ensure compile-package is run first.
(ert-run-tests-batch-and-exit '(or compile-package t))

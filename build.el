(princ (emacs-version))

(let ((generated-autoload-file (concat default-directory "autoloads.el")))
  (update-directory-autoloads "."))
(load-file "autoloads.el")
(add-to-list 'load-path ".")

(if (locate-library "ert")
    (require 'ert)
  (defmacro ert-deftest (name args &rest body)
    `(progn ,@body))
  (defun ert-run-tests-batch-and-exit ()
    (kill-emacs 0))
  (defun should (arg))
  (defun should-not (arg)))

(ert-deftest compile-package ()
  "Compile package."
  (should-not (string-match "failed" (byte-recompile-directory "." 0))))

(ert-deftest load-forth-mode ()
  "Load forth-mode."
  (should (require 'forth-mode))
  (should (featurep 'forth-mode))
  (with-temp-buffer
    (forth-mode)
    (should (eq major-mode 'forth-mode))
    (kill-buffer)))

(ert-run-tests-batch-and-exit)

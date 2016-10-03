(ert-deftest load-forth-mode ()
  "Load forth-mode."
  (should (require 'forth-mode))
  (should (featurep 'forth-mode))
  (with-temp-buffer
    (forth-mode)
    (should (eq major-mode 'forth-mode))
    (kill-buffer)))

(ert-deftest load-not-block ()
  (find-file "test/noblock.fth")
  (should (eq major-mode 'forth-mode))
  (should-not (and (boundp 'forth-block-mode) forth-block-mode)))

(ert-deftest load-block-with-newlines ()
  (find-file "test/block2.fth")
  (should (eq major-mode 'forth-mode))
  (should (and (boundp 'forth-block-mode) forth-block-mode)))

(ert-deftest load-block-without-newlines ()
  (find-file "test/block1.fth")
  (should (eq major-mode 'forth-mode))
  (should (and (boundp 'forth-block-mode) forth-block-mode)))

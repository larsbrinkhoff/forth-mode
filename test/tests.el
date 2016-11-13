(ert-deftest load-not-block ()
  (find-file "test/noblock.fth")
  (should (eq major-mode 'forth-mode))
  (should-not (and (boundp 'forth-block-mode) forth-block-mode))
  (kill-buffer))

(ert-deftest load-block-with-newlines ()
  (find-file "test/block2.fth")
  (should (eq major-mode 'forth-mode))
  (should (and (boundp 'forth-block-mode) forth-block-mode))
  (kill-buffer))

(ert-deftest load-block-without-newlines ()
  (find-file "test/block1.fth")
  (should (eq major-mode 'forth-mode))
  (should (and (boundp 'forth-block-mode) forth-block-mode))
  (kill-buffer))

(defmacro forth-with-temp-buffer (contents &rest body)
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (forth-mode)
     ,@body))

(defun forth-assert-face (content pos face)
  (forth-with-temp-buffer content
    (font-lock-fontify-buffer)
    (should (eq face (get-text-property pos 'face)))))

(ert-deftest forth-paren-comment-font-lock ()
  (forth-assert-face "( )" 1 font-lock-comment-delimiter-face)
  (forth-assert-face ".( )" 1 nil)
  (forth-assert-face "( )" 3 font-lock-comment-delimiter-face)
  (forth-assert-face " ( )" 2 font-lock-comment-delimiter-face)
  (forth-assert-face "\t( )" 2 font-lock-comment-delimiter-face)
  (forth-assert-face "(\t)" 1 font-lock-comment-delimiter-face)
  (forth-assert-face "(foo) " 3 nil)
  (forth-assert-face "( foo) " 3 font-lock-comment-face)
  (forth-assert-face "( a b --
                        x y )" 1 font-lock-comment-delimiter-face))

(ert-deftest forth-backslash-comment-font-lock ()
  (forth-assert-face "\\ " 1 font-lock-comment-delimiter-face)
  (forth-assert-face " \\ " 2 font-lock-comment-delimiter-face)
  (forth-assert-face "\t\\ " 2 font-lock-comment-delimiter-face)
  (forth-assert-face "a\\b " 2 nil))

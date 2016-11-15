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

(unless (boundp 'font-lock-ensure)
  ;; Emacs 24 doesn't have font-lock-ensure.
  (defun font-lock-ensure ()
    (font-lock-fontify-buffer)))

(defun forth-assert-face (content pos face)
  (when (boundp 'syntax-propertize-function)
    (forth-with-temp-buffer content
      (font-lock-ensure)
      (should (eq face (get-text-property pos 'face))))))

(defun forth-strip-| (string)
  (replace-regexp-in-string "^[ \t]*|" "" (substring-no-properties  string)))

(defun forth-should-indent (expected &optional content)
  "Assert that CONTENT turns into EXPECTED after the buffer is re-indented.
If CONTENT is not supplied uses EXPECTED as input.
The whitespace before and including \"|\" on each line is removed."
  (let ((content (or content expected)))
    (forth-with-temp-buffer (forth-strip-| content)
      (let ((inhibit-message t)) ; Suppress "Indenting region ... done" message
	(indent-region (point-min) (point-max)))
      ;; TODO: Can we check for a missing function in Emacs 23?
      (unless (version< emacs-version "24")
	(should (string= (forth-strip-| expected)
			 (substring-no-properties (buffer-string))))))))

(defun forth-assert-forward-sexp (content start end)
  (forth-with-temp-buffer content
    (goto-char start)
    (forward-sexp)
    (should (= (point) end))))

(defun forth-assert-forward-word (content start end)
  (forth-with-temp-buffer content
    (goto-char start)
    (font-lock-ensure) ; Make sure syntax-propertize function is called
    (forward-word)
    (should (= (point) end))))

(ert-deftest forth-paren-comment-font-lock ()
  (forth-assert-face "( )" 1 font-lock-comment-delimiter-face)
  (forth-assert-face ".( )" 1 font-lock-comment-face)
  (forth-assert-face "( )" 3 font-lock-comment-delimiter-face)
  (forth-assert-face " ( )" 2 font-lock-comment-delimiter-face)
  (forth-assert-face "\t( )" 2 font-lock-comment-delimiter-face)
  (forth-assert-face "(\t)" 1 font-lock-comment-delimiter-face)
  (forth-assert-face "(foo) " 3 nil)
  (forth-assert-face "(foo)" 3 nil)
  (forth-assert-face "() " 2 nil)
  (forth-assert-face "( foo) " 3 font-lock-comment-face)
  (forth-assert-face "( a b --
                        x y )" 1 font-lock-comment-delimiter-face))

(ert-deftest forth-backslash-comment-font-lock ()
  (forth-assert-face "\\" 1 nil)
  (forth-assert-face "\\ " 1 font-lock-comment-delimiter-face)
  (forth-assert-face " \\" 2 nil)
  (forth-assert-face "\t\\ " 2 font-lock-comment-delimiter-face)
  (forth-assert-face " \\\t" 2 font-lock-comment-delimiter-face)
  (forth-assert-face " \\\n" 2 font-lock-comment-delimiter-face)
  (forth-assert-face "a\\b" 2 nil)
  (forth-assert-face "a\\b " 2 nil))

(ert-deftest forth-brace-colon-font-lock ()
  (forth-assert-face "{: :}" 1 font-lock-comment-face)
  (forth-assert-face "{: :}" 5 font-lock-comment-face)
  (forth-assert-face "{: a b :}" 4 font-lock-comment-face)
  (forth-assert-face "{::}" 1 nil)
  (forth-assert-face "{: a b --
                         x y :}" 1 font-lock-comment-face)
  (forth-assert-face "t{ 2 1+ -> 3 }t" 2 nil))

(ert-deftest forth-string-font-lock ()
  (forth-assert-face "s\" ab\"" 1 nil)
  (forth-assert-face "s\" ab\"" 2 font-lock-string-face)
  (forth-assert-face "abort\" ab\"" 6 font-lock-string-face)
  (forth-assert-face ".\" ab\"" 2 font-lock-string-face)
  (forth-assert-face "c\" ab\"" 2 font-lock-string-face)
  (forth-assert-face "[char] \" of" 10 nil)
  (forth-assert-face "frob\" ab\" " 6 nil)
  (forth-assert-face "s\\\" ab\"" 1 nil)
  (forth-assert-face "s\\\" ab\"" 3 font-lock-string-face)
  (forth-assert-face "s\\\" ab\"" 6 font-lock-string-face)
  (forth-assert-face "s\\\" a\\\"c\"" 7 font-lock-string-face)
  (forth-assert-face "s\\\" a\\\"c\" x" 10 nil))

(ert-deftest forth-parsing-words-font-lock ()
  (forth-assert-face "postpone ( x " 11 nil)
  (forth-assert-face "' s\" x " 6 nil))

(ert-deftest forth-indent-colon-definition ()
  (forth-should-indent
   ": foo ( x y -- y x )
   |  swap
   |;"))

(ert-deftest forth-indent-if-then-else ()
  (forth-should-indent
   "x if
   |  3 +
   |then")
  (forth-should-indent
   "x if
   |  3 +
   |else
   |  1+
   |then")
  (forth-should-indent
   "x IF
   |  3 +
   |ELSE
   |  1+
   |THEN"))

(ert-deftest forth-indent-begin-while-repeat ()
  (forth-should-indent
   "begin
   |  0>
   |while
   |  1-
   |repeat")
  (forth-should-indent
   "begin
   |  0>
   |while
   |  begin
   |    foo
   |  while
   |    bar
   |  repeat
   |  1-
   |repeat"))

;; FIXME: this kind of code is indented poorly (difficult for SMIE)
;; |: foo ( )
;; |  begin
;; |    bar while
;; |    baz while
;; |again then then ;

(ert-deftest forth-indent-do ()
  (forth-should-indent
   "10 0 ?do
   |  .
   |loop")
  (forth-should-indent
   "10 0 ?do
   |  . 2
   |+loop"))

(ert-deftest forth-indent-case ()
  (forth-should-indent
   "x case
   |  [char] f of
   |    foo
   |  endof
   |  [char] b of bar
   |              baz
   |           endof
   |  drop exit
   |endcase"))

(ert-deftest forth-sexp-movements ()
  (forth-assert-forward-sexp " : foo bar ; \ x" 2 13)
  (forth-assert-forward-sexp " :noname foo bar ; \ x" 2 19)
  (forth-assert-forward-sexp " if drop exit else 1+ then bar " 2 27))

;; IDEA 1: in words like foo-bar give the "-" a "word constituent"
;; syntax so that word movement works like in Lisp mode (which
;; everybody is used to :-).
;;
;; IDEA 2: give the filename in "include filename" string syntax.
(ert-deftest forth-word-movements ()
  (forth-assert-forward-word "include /tmp/foo.fth \ bar" 1 8)
  (forth-assert-forward-word "include /tmp/foo.fth \ bar" 8 21))

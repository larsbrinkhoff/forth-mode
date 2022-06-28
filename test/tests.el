(require 'forth-mode)
(require 'forth-interaction-mode)
(require 'forth-block-mode)

(unless forth-executable
  (setq forth-executable (getenv "FORTH")))

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

(defun forth-strip-| (string)
  (replace-regexp-in-string "^[ \t]*|" "" (substring-no-properties  string)))

(defun forth-strip-|-and-→ (string)
  (let* ((s2 (forth-strip-| string))
	 (pos (1+ (string-match "→" s2))))
    (list (remove ?→ s2) pos)))

(defun forth-strip-|-and-¹² (string)
  (let* ((s2 (forth-strip-| string))
	 (start (1+ (string-match "¹" (remove ?² s2))))
	 (end (1+ (string-match "²" (remove ?¹ s2)))))
    (list (remove ?² (remove ?¹ s2))
	  start end)))

(defun forth-assert-face (content face)
  (when (boundp 'syntax-propertize-function)
    (cl-destructuring-bind (content pos) (forth-strip-|-and-→ content)
      (forth-with-temp-buffer content
	(font-lock-ensure)
	(should (eq face (or (get-text-property pos 'face)
			     (get-text-property pos 'font-lock-face))))))))

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

(defun forth-assert-forward-sexp (content)
  (cl-destructuring-bind (content start end) (forth-strip-|-and-¹² content)
    (forth-with-temp-buffer content
      (goto-char start)
      (forward-sexp)
      (should (= (point) end)))))

(defun forth-assert-forward-word (content)
  (cl-destructuring-bind (content start end) (forth-strip-|-and-¹² content)
    (forth-with-temp-buffer content
      (goto-char start)
      (font-lock-ensure) ; Make sure syntax-propertize function is called
      (forward-word)
      (should (= (point) end)))))

(defun forth-should-before/after (before after fun)
  (cl-destructuring-bind (before point-before) (forth-strip-|-and-→ before)
    (cl-destructuring-bind (after point-after) (forth-strip-|-and-→ after)
      (forth-with-temp-buffer before
	(goto-char point-before)
	(funcall fun)
	(should (string= after (substring-no-properties (buffer-string))))
	(should (= (point) point-after))))))

(defun forth-should-region-before/after (before after fun)
  (cl-destructuring-bind (before start1 end1) (forth-strip-|-and-¹² before)
    (cl-destructuring-bind (after point-after) (forth-strip-|-and-→ after)
      (forth-with-temp-buffer before
	(set-mark start1)
	(goto-char end1)
	(activate-mark)
	(funcall fun)
	(should (string= after (substring-no-properties (buffer-string))))
	(should (= (point) point-after))))))

(defmacro forth-with-forth (&rest body)
  (declare (indent 0))
  `(let* ((proc (get-buffer-process forth-interaction-buffer)))
     ;; FIXME: there should be a better way to do this. Probably a
     ;; callback function.
     (while (not (processp proc))
       (run-forth)
       (message "Waiting for Forth to start ...")
       (accept-process-output nil 0.3)
       (setq proc (get-buffer-process forth-interaction-buffer)))
     (unwind-protect
	 (progn . ,body)
	 (kill-process proc))))

(defun forth-assert-backward-token (content token)
  (cl-destructuring-bind (content pos1 pos2) (forth-strip-|-and-¹² content)
    (forth-with-temp-buffer content
      (goto-char pos1)
      (let ((token2 (forth-smie--backward-token)))
	(should (equal token2 token))
	(should (= (point) pos2))))))

(defun forth-assert-forward-token (content token)
  (cl-destructuring-bind (content pos1 pos2) (forth-strip-|-and-¹² content)
    (forth-with-temp-buffer content
      (goto-char pos1)
      (let ((token2 (forth-smie--forward-token)))
	(should (equal token2 token))
	(should (= (point) pos2))))))

(ert-deftest forth-paren-comment-font-lock ()
  (forth-assert-face "→( )" font-lock-comment-delimiter-face)
  (forth-assert-face "→.( )" font-lock-comment-face)
  (forth-assert-face "( →)" font-lock-comment-delimiter-face)
  (forth-assert-face " →( )" font-lock-comment-delimiter-face)
  (forth-assert-face "\t→( )" font-lock-comment-delimiter-face)
  (forth-assert-face "→(\t)" font-lock-comment-delimiter-face)
  (forth-assert-face "(fo→o) " nil)
  (forth-assert-face "(fo→o)" nil)
  (forth-assert-face "(→) " nil)
  (forth-assert-face "( →foo) " font-lock-comment-face)
  (forth-assert-face "( a b --
                        →x y )" font-lock-comment-face))

(ert-deftest forth-backslash-comment-font-lock ()
  (forth-assert-face "→\\" font-lock-comment-face)
  (forth-assert-face "→\\ " font-lock-comment-delimiter-face)
  (forth-assert-face " →\\" font-lock-comment-face)
  (forth-assert-face "\t→\\ " font-lock-comment-delimiter-face)
  (forth-assert-face " →\\\t" font-lock-comment-delimiter-face)
  (forth-assert-face " →\\\n" font-lock-comment-face)
  (forth-assert-face "a→\\b" nil)
  (forth-assert-face "a→\\b " nil))

(ert-deftest forth-brace-colon-font-lock ()
  (forth-assert-face "→{: :}" font-lock-comment-face)
  (forth-assert-face "{: :→}" font-lock-comment-face)
  (forth-assert-face "{: →a b :}" font-lock-comment-face)
  (forth-assert-face "→{::}" nil)
  (forth-assert-face "{: a b --
                         →x y :}" font-lock-comment-face)
  (forth-assert-face "t→{ 2 1+ -> 3 }t" nil))

(ert-deftest forth-string-font-lock ()
  (forth-assert-face "→s\" ab\"" nil)
  (forth-assert-face "s→\" ab\"" font-lock-string-face)
  (forth-assert-face "abort→\" ab\"" font-lock-string-face)
  (forth-assert-face ".→\" ab\"" font-lock-string-face)
  (forth-assert-face "c→\" ab\"" font-lock-string-face)
  (forth-assert-face "[char] \" →swap" nil)
  (forth-assert-face "frob\" →ab\" " nil)
  (forth-assert-face "s\" →a \n b " font-lock-string-face)
  (forth-assert-face "s\" a \n →b " nil)
  (forth-assert-face "→s\\\" ab\"" nil)
  (forth-assert-face "s\\→\" ab\"" font-lock-string-face)
  (forth-assert-face "s\\\" a→b\"" font-lock-string-face)
  (forth-assert-face "s\\\" a\\\"→c\"" font-lock-string-face)
  (forth-assert-face "s\\\" \\\\ →a \" b" font-lock-string-face)
  (forth-assert-face "s\\\" \\\\ a \" →b" nil)
  (forth-assert-face "s\\\" \\\" →a \" b" font-lock-string-face)
  (forth-assert-face "s\\\" \\\" a \" →b" nil)
  (forth-assert-face "s\\\" →a \n b " font-lock-string-face)
  (forth-assert-face "s\\\" a \n →b " nil))

(ert-deftest forth-parsing-words-font-lock ()
  (forth-assert-face "postpone ( →x " nil)
  (forth-assert-face "' s\" →x "nil)
  (forth-assert-face "case [char] ' →of exit endof " font-lock-keyword-face)
  (forth-assert-face "case [char] ' →?of exit endof " font-lock-keyword-face)
  (forth-assert-face "→postpone postpone" font-lock-keyword-face)
  (forth-assert-face "postpone →postpone" nil)
  (forth-assert-face "→literal" font-lock-keyword-face)
  (forth-assert-face "postpone →literal" nil)
  (forth-assert-face "[ 48 ] →literal" font-lock-keyword-face)
  (forth-assert-face "→: frob ;" font-lock-keyword-face)
  (forth-assert-face ": →frob ;" font-lock-function-name-face)
  (forth-assert-face "constant →foo" font-lock-function-name-face)
  (forth-assert-face "create →foo" font-lock-function-name-face)
  (forth-assert-face "value →foo" font-lock-function-name-face)
  (forth-assert-face "variable →foo" font-lock-function-name-face)
  (forth-assert-face "synonym →foo bar" font-lock-function-name-face))

(ert-deftest forth-indent-colon-definition ()
  (forth-should-indent
   ": foo ( x y -- y x )
   |  swap
   |;")
  ;; Open Firmware style
  (let ((forth-smie-basic-indent 3))
    (forth-should-indent
     ": foo ( x y -- y x )
     |   swap
     |;")))

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
   |  test ?of
   |  drop exit
   |endcase"))

(ert-deftest forth-indent-customization ()
  (forth-should-indent
   "\ -*- forth-smie-bnf-extensions: ((ext (\"?of\" words \"endof\"))) -*-
   |x case
   |  [char] f of
   |    foo
   |  endof
   |  test ?of
   |    bar
   |  endof
   |endcase"))

;; This is an tricky case because SMIE thinks, depending on
;; `comment-start-skip` (which indirectly depends on `comment-start`
;; thru `comment-normalize-vars`), that (foo) is a comment.  But since
;; (foo) is not actually a comment this leads to an endless recursion.
(ert-deftest forth-indent-\(foo\) ()
  (forth-should-indent
   ": foo
   |  (foo) ;"))

(ert-deftest forth-indent-structure ()
  (forth-should-indent
   "BEGIN-STRUCTURE point
   |  1 CELLS +FIELD p.x
   |  1 CELLS +FIELD p.y
   |END-STRUCTURE"))

(ert-deftest forth-indent-noname ()
  (forth-should-indent
   "1 2 :noname
   |      swap
   |    ;
   |execute"))

(ert-deftest forth-indent-postpone ()
  (forth-should-indent
   ": foo
   |  postpone :
   |  42 postpone literal
   |  postpone ;
   |;")
  (forth-should-indent
   ": foo
   |  POSTPONE :
   |  42 POSTPONE literal
   |  postpone ;
   |;")
  (forth-should-indent
   ": foo
   |  postpone if
   |  if
   |    postpone then
   |  else
   |    postpone then
   |  then
   |;")
  (forth-should-indent
   ": foo
   |  ['] :
   |  if
   |    postpone ;
   |  else
   |    postpone recurse postpone ;
   |  then
   |;")
  )

(ert-deftest forth-sexp-movements ()
  (forth-assert-forward-sexp " ¹: foo bar ;² \ x")
  (forth-assert-forward-sexp " ¹:noname foo bar ;² \ x")
  (forth-assert-forward-sexp " ¹if drop exit else 1+ then² bar ")
  (forth-assert-forward-sexp " : foo ¹postpone if² postpone then ;"))

;; IDEA: give the filename in "include filename" string syntax.
(ert-deftest forth-word-movements ()
  (forth-assert-forward-word "¹include² /tmp/foo.fth \ bar")
  (forth-assert-forward-word "include¹ /tmp²/foo.fth \ bar")
  (forth-assert-forward-word "¹foo²-bar"))

(ert-deftest forth-spec-parsing ()
  (should (equal (forth-spec--build-url "SWAP" 1994)
		 "http://lars.nocrew.org/dpans/dpans6.htm#6.1.2260")))

(ert-deftest forth-fill-comment ()
  (forth-should-before/after
   "\\ foo bar
   |\\ baz→
   |: frob ( x y -- z ) ;"
   "\\ foo bar baz→
   |: frob ( x y -- z ) ;"
   #'fill-paragraph))

(ert-deftest forth-beginning-of-defun ()
  (forth-should-before/after
   ": foo bar ;
   |: baz ( x -- )
   |  if foo→ then ;"
   ": foo bar ;
   |→: baz ( x -- )
   |  if foo then ;"
   #'beginning-of-defun))

;; FIXME: maybe insert "(  )" instead of "()".
(ert-deftest forth-comment-dwim ()
  (forth-should-before/after
   ": frob
   |  begin     ( x y )
   |    swap→
   |  again ;"
   ": frob
   |  begin     ( x y )
   |    swap    ( → )
   |  again ;"
   (lambda ()
     (call-interactively #'comment-dwim)))
  (forth-should-region-before/after
   "²: frob
   |  begin     ( x y )
   |    swap
   |  again ;
   |¹"
   "→\\ : frob
   |\\   begin     ( x y )
   |\\     swap
   |\\   again ;
   |"
   (lambda ()
     (call-interactively #'comment-dwim)))
  (forth-should-region-before/after
   "¹\\ : frob
   |\\   begin     ( x y )
   |\\     swap
   |\\   again ;
   |²"
   ": frob
   |  begin     ( x y )
   |    swap
   |  again ;
   |→"
   (lambda ()
     (call-interactively #'comment-dwim))))

(ert-deftest forth-completion-at-point ()
  (forth-with-forth
    (forth-should-before/after
     "2c→"
     "2Constant→"
     #'completion-at-point)))

(ert-deftest forth-smie-backward-token ()
  (forth-assert-backward-token "²foo¹" "foo")
  (forth-assert-backward-token "²foo-bar¹" "foo-bar")
  (forth-assert-backward-token "  ²foo-bar  ¹baz" "foo-bar")
  (forth-assert-backward-token " ²?#!-+  ¹" "?#!-+")
  (forth-assert-backward-token " ²foo ( x y ) ¹" "foo")
  (forth-assert-backward-token " foo \ x ²y  ¹" "y")
  (forth-assert-backward-token " ²postpone foo¹" '("postpone" "foo"))
  (forth-assert-backward-token " ²[']   foo ¹" '("[']" "foo"))
  (forth-assert-backward-token " ²[char]  : ¹" '("[char]" ":"))
  ;; We're mostly interested in getting indentation inside colon
  ;; definitions right, so here we don't treat ' as parsing word.
  (forth-assert-backward-token " ' ²foo¹" "foo")
  (forth-assert-backward-token " : ²foo¹" "foo"))

(ert-deftest forth-smie-forward-token ()
  (forth-assert-forward-token "¹foo²" "foo")
  (forth-assert-forward-token "¹foo-bar²" "foo-bar")
  (forth-assert-forward-token "  ¹foo-bar²  baz" "foo-bar")
  (forth-assert-forward-token " ¹?#!-+²  " "?#!-+")
  (forth-assert-forward-token " ¹foo² ( x y )" "foo")
  (forth-assert-forward-token " foo \ x ¹y² " "y")
  (forth-assert-forward-token " ¹postpone foo²" '("postpone" "foo"))
  (forth-assert-forward-token " ¹ [']   foo² " '("[']" "foo"))
  (forth-assert-forward-token " ¹[char]  :² " '("[char]" ":"))
  (forth-assert-forward-token " ¹'² foo" "'")
  (forth-assert-forward-token " ¹:² foo" ":"))

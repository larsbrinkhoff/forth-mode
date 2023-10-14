;;; forth-syntax.el -- syntax-propertize function       -*-lexical-binding:t-*-

;; This code mimics the Forth text interpreter and adds text
;; properties as side effect.

(require 'cl-lib)


;;; Helpers

(defvar forth-syntax-whitespace " \t\n\f\r")
(defvar forth-syntax-non-whitespace (concat "^" forth-syntax-whitespace))

;; Skip forward over whitespace and the following word. Return the
;; start position of the word.
(defun forth-syntax--skip-word ()
  (skip-chars-forward forth-syntax-whitespace)
  (let ((start (point)))
    (skip-chars-forward forth-syntax-non-whitespace)
    start))

;; Return the whitespace-delimited word at position POS.
;; Return nil if POS is at end-of-buffer.
(defun forth-syntax--word-at (pos)
  (save-excursion
    (goto-char pos)
    (let ((start (forth-syntax--skip-word)))
      (cond ((= start (point)) nil)
	    (t (buffer-substring-no-properties start (point)))))))

(defmacro forth-syntax--set-syntax (start end syntax)
  "Set the 'syntax-table property in the region START/END to SYNTAX.
SYNTAX must be a valid argument for `string-to-syntax'."
  `(put-text-property ,start ,end 'syntax-table ',(string-to-syntax syntax)))

;; Set the syntax in the region START/END to "word" or "symbol".  Do
;; nothing for characters that already have the correct syntax so that
;; word movement commands work "naturally".
(defun forth-syntax--set-word-syntax (start end)
  (save-excursion
    (goto-char start)
    (while (progn
	     (skip-syntax-forward "w_" end)
	     (cond ((< (point) end)
		    (let ((start (point)))
		      (skip-syntax-forward "^w_" end)
		      (forth-syntax--set-syntax start (point) "_")
		      t))
		   (t nil))))))


;;; State functions

;; The parser is a loop that calls "state-functions".
;; A state function parses forward from point, adds text-properties as needed,
;; and returns the next state-function.
;;
;; The naming convention for state-functions is forth-syntax--state-FOO.

(defun forth-syntax--state-eob ()
  (cl-assert (eobp))
  (error "This state function should never be called"))

;; One line strings
(defun forth-syntax--state-string ()
  (forth-syntax--set-syntax (1- (point)) (point) "|")
  (cond ((re-search-forward "[\"\n]" nil t)
	 (forth-syntax--set-syntax (1- (point)) (point) "|")
	 #'forth-syntax--state-normal)
	(t
	 (goto-char (point-max))
	 #'forth-syntax--state-eob)))

(defun forth-syntax--state-s\\\" ()
  (forth-syntax--set-syntax (1- (point)) (point) "|")
  (while (and (re-search-forward "\\([\"\n]\\|\\\\\\\\\\|\\\\\"\\)" nil t)
	      (cond ((= (char-after (match-beginning 0)) ?\\)
		     (forth-syntax--set-syntax (match-beginning 0)
					       (1+ (match-beginning 0))
					       "\\")
		     t))))
  (cond ((looking-back "[\"\n]" 1)
	 (forth-syntax--set-syntax (1- (point)) (point) "|")
	 #'forth-syntax--state-normal)
	(t
	 (goto-char (point-max))
	 #'forth-syntax--state-eob)))

;; The position where the current word started.  It is setup by
;; `forth-syntax--state-normal'.  It avoids the need to scan backward
;; so often.
(defvar forth-syntax--current-word-start -1)

;; For the word before point, set the font-lock-face property.
(defun forth-syntax--mark-font-lock-keyword ()
  (let ((start forth-syntax--current-word-start))
    (put-text-property start (point) 'font-lock-face font-lock-keyword-face)))

(defun forth-syntax--state-font-lock-keyword ()
  (forth-syntax--mark-font-lock-keyword)
  (forth-syntax--state-normal))


;; State for words that parse the following word, e.g. POSTPONE S"
;; where POSTPONE parses S".
;;
;; FIXME: It would nice be to know if we are in compilation state for
;; things like this: : FOO CREATE , ;
;; Because in this case CREATE doesn't parse immediately.
(defun forth-syntax--state-parsing-word ()
  (let ((start (forth-syntax--skip-word)))
    (cond ((= start (point))
	   #'forth-syntax--state-eob)
	  (t
	   (forth-syntax--set-word-syntax start (point))
	   #'forth-syntax--state-normal))))

;; This is like `forth-syntax--state-parsing-word' but additionally
;; sets the font-lock-keyword-face.
(defun forth-syntax--state-parsing-keyword ()
  (forth-syntax--mark-font-lock-keyword)
  (forth-syntax--state-parsing-word))

;; This is also like `forth-syntax--state-parsing-word' but
;; additionally set font-lock-keyword-face for the current word and
;; font-lock-function-name-face for the following word.
;; It's intended for thigs like: DEFER S"
(defun forth-syntax--state-defining-word ()
  (forth-syntax--mark-font-lock-keyword)
  (let ((start (forth-syntax--skip-word)))
    (cond ((= start (point))
	   #'forth-syntax--state-eob)
	  (t
	   (forth-syntax--set-word-syntax start (point))
	   (put-text-property start (point) 'font-lock-face
			      font-lock-function-name-face)
	   #'forth-syntax--state-normal))))

(defun forth-syntax--parse-comment (backward-regexp forward-regexp)
  (let ((pos (point)))
    (re-search-backward backward-regexp)
    (forth-syntax--set-syntax (point) (1+ (point)) "!")
    (goto-char pos)
    (cond ((re-search-forward forward-regexp nil t)
	   (forth-syntax--set-syntax (1- (point)) (point) "!")
	   #'forth-syntax--state-normal)
	  (t
	   (goto-char (point-max))
	   #'forth-syntax--state-eob))))

;; Define a state-function for comments.  The comment starts with
;; the string BEGIN and ends with the string END.
(defmacro forth-syntax--define-comment-state (begin end)
  (let ((fname (intern (concat "forth-syntax--state-" begin))))
    `(defun ,fname ()
       (forth-syntax--parse-comment ,(concat (regexp-quote begin) "\\=")
				    ,(regexp-quote end)))))

(forth-syntax--define-comment-state "(" ")")
(forth-syntax--define-comment-state "\\" "\n")
(forth-syntax--define-comment-state ".(" ")")

;; For now, treat locals like comments
(forth-syntax--define-comment-state "{:" ":}")

;; Hashtable that maps strings (word names) to parser functions.
(defvar forth-syntax--parsers (make-hash-table :test 'equal))

(defun forth-syntax--define (word parsing-function)
  (setf (gethash (downcase word) forth-syntax--parsers) parsing-function))

;; Find the parsing function for WORD.
(defun forth-syntax--lookup (word)
  (gethash (downcase word) forth-syntax--parsers))

(forth-syntax--define "s\"" #'forth-syntax--state-string)
(forth-syntax--define ".\"" #'forth-syntax--state-string)
(forth-syntax--define "c\"" #'forth-syntax--state-string)
(forth-syntax--define "abort\"" #'forth-syntax--state-string)

(forth-syntax--define "s\\\"" #'forth-syntax--state-s\\\")

(forth-syntax--define "(" #'forth-syntax--state-\()
(forth-syntax--define "\\" #'forth-syntax--state-\\)
(forth-syntax--define ".(" #'forth-syntax--state-.\()
(forth-syntax--define "{:" #'forth-syntax--state-{:)

(forth-syntax--define "postpone" #'forth-syntax--state-parsing-keyword)

(defvar forth-syntax--parsing-words
  '("'" "[']" "char" "[char]"))

(defvar forth-syntax--defining-words
  '(":" "create" "synonym" "defer" "code"
    "constant" "2constant" "fconstant"
    "value" "2value" "fvalue"
    "variable" "2variable" "fvariable"
    "+field" "field:" "cfield:" "ffield:" "sffield:" "dffield:"
    ))

(defvar forth-syntax--font-lock-keywords
  '("if" "else" "then"
    "?do" "do" "unloop" "exit" "leave" "loop" "+loop"
    "begin" "while" "repeat" "again" "until"
    "case" "?of" "of" "endof" "endcase"
    ":noname" ";" "does>" "immediate"
    "is" "to"
    "literal" "2literal" "fliteral" "sliteral"
    "begin-structure" "end-structure"))

(dolist (w forth-syntax--parsing-words)
  (forth-syntax--define w #'forth-syntax--state-parsing-word))

(dolist (w forth-syntax--defining-words)
  (forth-syntax--define w #'forth-syntax--state-defining-word))

(dolist (w forth-syntax--font-lock-keywords)
  (forth-syntax--define w #'forth-syntax--state-font-lock-keyword))

;; Look for the next whitespace delimited word; mark all its
;; characters as "word constituents"; finally return state-function
;; for the word.
(defun forth-syntax--state-normal ()
  (let ((start (forth-syntax--skip-word)))
    (cond ((= start (point))
	   #'forth-syntax--state-eob)
	  (t
	   (forth-syntax--set-word-syntax start (point))
	   (let* ((word (buffer-substring-no-properties start (point)))
		  (parser (forth-syntax--lookup word)))
	     (cond (parser
		    (setq forth-syntax--current-word-start start)
		    (funcall parser))
		   (t
		    #'forth-syntax--state-normal)))))))


;;; Guess initial state

;; Is it normal that `syntax-ppss' moves point or is that a bug?
(defun forth-syntax--ppss (pos)
  (save-excursion
    (syntax-ppss pos)))

(defun forth-syntax--in-comment-p (pos)
  (not (null (elt (forth-syntax--ppss pos) 4))))

(defun forth-syntax--comment-start-position (pos)
  (elt (forth-syntax--ppss pos) 8))

;; Make a guess for the syntax state at position POS.
;; Return a pair (START .  PARSING-FUNCTION).
(defun forth-syntax--guess-state (pos)
    (cond ((and (< (point-min) pos)
		(forth-syntax--in-comment-p (1- pos)))
	   (cons (forth-syntax--comment-start-position (1- pos))
		 #'forth-syntax--state-normal))
	  (t
	   (cons pos #'forth-syntax--state-normal))))


;;; Main entry point

;; Guess a state for the position START, then call state-functions
;; until the position END is reached.
(defun forth-syntax-propertize (start end)
  (save-excursion
    (remove-text-properties start end '(font-lock-face))
    (let* ((guess (forth-syntax--guess-state start))
	   (state (cdr guess)))
      ;;(message "forth-syntax-propertize: %s %s %s" start end guess)
      (goto-char (car guess))
      (while (< (point) end)
	(let ((start (point)))
	  (setq state (funcall state))
	  (cl-assert (< start (point))))))))

(provide 'forth-syntax)

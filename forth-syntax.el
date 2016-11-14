;; forth-syntax.el -- syntax-propertize function for forth-mode

;; This code mimics the Forth text interpreter and adds text
;; properties as side effect.

(require 'cl)


;;; Helpers

(defvar forth-syntax--whitespace " \t\n\f\r")
(defvar forth-syntax--non-whitespace (concat "^" forth-syntax--whitespace))

;; Return the whitespace-delimited word at position POS.
;; Return nil if POS is at end-of-buffer.
(defun forth-syntax--word-at (pos)
  (save-excursion
    (goto-char pos)
    (skip-chars-forward forth-syntax--whitespace)
    (let ((start (point)))
      (skip-chars-forward forth-syntax--non-whitespace)
      (cond ((= start (point)) nil)
	    (t (buffer-substring-no-properties start (point)))))))



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
  (re-search-backward "\"\\=")
  (put-text-property (point) (1+ (point))
		     'syntax-table (string-to-syntax "\""))
  (forward-char)
  (cond ((re-search-forward "[\"\n]" nil t)
	 (put-text-property (1- (point)) (point)
			    'syntax-table (string-to-syntax "\""))
	 #'forth-syntax--state-normal)
	(t
	 (goto-char (point-max))
	 #'forth-syntax--state-normal)))

(defun forth-syntax--state-s\\\" ()
  (re-search-backward "\"\\=")
  (put-text-property (point) (1+ (point))
		     'syntax-table (string-to-syntax "\""))
  (forward-char)
  (while (and (re-search-forward "\\([\"\n]\\|\\\\\"\\)" nil t)
	      (cond ((= (char-after (match-beginning 0)) ?\\)
		     (put-text-property (match-beginning 0)
					(1+ (match-beginning 0))
					'syntax-table (string-to-syntax "\\"))
		     t))))
  (cond ((looking-back "[\"\n]" 1)
	 (put-text-property (1- (point)) (point)
			    'syntax-table (string-to-syntax "\""))
	 #'forth-syntax--state-normal)
	(t
	 (goto-char (point-max))
	 #'forth-syntax--state-normal)))

;; State for words that parse the following word, e.g. POSTPONE S"
;; where POSTPONE parses S".
(defun forth-syntax--state-parsing-word ()
  (skip-chars-forward forth-syntax--whitespace)
  (let ((start (point)))
    (skip-chars-forward forth-syntax--non-whitespace)
    (cond ((= start (point)) #'forth-syntax--state-eob)
	  (t
	   (put-text-property start (point)
			      'syntax-table (string-to-syntax "_"))
	   #'forth-syntax--state-normal))))

(defun forth-syntax--parse-comment (backward-regexp forward-regexp)
  (let ((pos (point)))
    (re-search-backward backward-regexp)
    (put-text-property (point) (1+ (point))
		       'syntax-table (string-to-syntax "<"))
    (goto-char pos)
    (cond ((re-search-forward forward-regexp nil t)
	   (put-text-property (1- (point)) (point)
			      'syntax-table (string-to-syntax ">"))
	   #'forth-syntax--state-normal)
	  (t
	   (goto-char (point-max))
	   #'forth-syntax--state-normal))))

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

(forth-syntax--define "s\"" #'forth-syntax--state-string)
(forth-syntax--define ".\"" #'forth-syntax--state-string)
(forth-syntax--define "c\"" #'forth-syntax--state-string)
(forth-syntax--define "abort\"" #'forth-syntax--state-string)

(forth-syntax--define "s\\\"" #'forth-syntax--state-s\\\")

(forth-syntax--define "(" #'forth-syntax--state-\()
(forth-syntax--define "\\" #'forth-syntax--state-\\)
(forth-syntax--define ".(" #'forth-syntax--state-.\()
(forth-syntax--define "{:" #'forth-syntax--state-{:)

(forth-syntax--define "postpone" #'forth-syntax--state-parsing-word)
(forth-syntax--define "'" #'forth-syntax--state-parsing-word)
(forth-syntax--define "[']" #'forth-syntax--state-parsing-word)
(forth-syntax--define ":" #'forth-syntax--state-parsing-word)

;; Find the parsing function for WORD.
(defun forth-syntax--lookup (word)
  (gethash (downcase word) forth-syntax--parsers))

;; Look for the next whitespace delimited word; mark all its
;; characters as "symbol constituents"; finally return state-function
;; for the word.
(defun forth-syntax--state-normal ()
  (skip-chars-forward forth-syntax--whitespace)
  (let ((start (point)))
    (skip-chars-forward forth-syntax--non-whitespace)
    (cond ((= start (point)) #'forth-syntax--state-eob)
	  (t
	   (put-text-property start (point)
			      'syntax-table (string-to-syntax "_"))
	   (let ((word (buffer-substring-no-properties start (point))))
	     (cond ((forth-syntax--lookup word))
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
    (let* ((guess (forth-syntax--guess-state start))
	   (state (cdr guess)))
      ;;(message "forth-syntax-propertize: %s %s %s" start end guess)
      (goto-char (car guess))
      (while (< (point) end)
	(let ((start (point)))
	  (setq state (funcall state))
	  (cl-assert (< start (point))))))))

(provide 'forth-syntax)

;; SMIE based indentation

(require 'smie)

(defcustom forth-smie-basic-indent 2
  "Basic amount of indentation."
  :type 'integer
  :group 'forth-smie
  :safe 'integerp)

(defcustom forth-smie-bnf-extensions '()
  "Rules for non-standard syntax.

We add this list of BNF rules to the default rules to support
user defined syntax.  E.g., setting this variable to

  '((gforth-ext (\"?of\" words \"endof\")))

tells Emacs to recognize ?OF ... ENDOF as a matching pair of tokens.

This variable can also be set in .dir-locals.el, e.g.:

  ((forth-mode . ((forth-smie-bnf-extensions
		 . ((my-stuff (\"import\" words \"{\" words \"}\"))))))).
"
  :type '(alist :key-type symbol :value-type (list (list string symbol)))
  :group 'forth-smie
  :safe 'listp)

(defconst forth-smie--bnf
  '((control
     ("if" words "else" words "then")
     ("if" words "then")
     ("begin" words "while" words "repeat")
     ("begin" words "until")
     ("begin" words "again")
     ("of" words "endof")
     ("case" words "endcase")
     ("?do" words "loop")
     ("?do" words "+loop")
     ("do" words "loop")
     ("do" words "+loop")
     ("begin-structure" words "end-structure")
     (":" words ";")
     (":noname" words ";"))
    (words)))

(defun forth-smie--grammar ()
  (smie-prec2->grammar
   (smie-bnf->prec2 (append forth-smie--bnf
			    forth-smie-bnf-extensions))))

(unless (fboundp 'pcase)
  (defmacro pcase (form &rest forms)
    0))

(defun forth-smie--indentation-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) forth-smie-basic-indent)
    (`(:elem . args)
     (cond ((smie-rule-prev-p ":" "begin-structure")
            (- (+ (save-excursion
		    (forth-smie--backward-token)
		    (current-column))
                  forth-smie-basic-indent)
               (current-column)))
           (t 0)))
    (`(:after . ":")               (* 2 forth-smie-basic-indent))
    (`(:after . "begin-structure") (* 2 forth-smie-basic-indent))
    (`(:before . ":noname") (cond ((smie-rule-hanging-p)
				   (current-column))
				  (t nil)))
    (`(:list-intro . ":")               nil)
    (`(:list-intro . "begin-structure") nil)
    (`(:list-intro . ,_) t)
    (_ nil)))

(defconst forth-smie--parsing-word-regexp
  (eval-when-compile
    (concat "^"
	    (regexp-opt '("postpone" "[']" "[char]"))
	    "$")))

(defun forth-smie--forward-word ()
  (let* ((start (progn (skip-syntax-forward " ") (point)))
	 (end (progn (skip-syntax-forward "w_") (point))))
    (buffer-substring-no-properties start end)))

(defun forth-smie--backward-word ()
  (let* ((end (progn (skip-syntax-backward " ") (point)))
	 (start (progn (skip-syntax-backward "w_") (point))))
    (buffer-substring-no-properties start end)))

(defun forth-smie--forward-token ()
  (forward-comment (point-max))
  (let* ((word1 (downcase (forth-smie--forward-word)))
	 (pos1 (point))
	 (word2 (downcase (forth-smie--forward-word))))
    (cond ((string-match forth-smie--parsing-word-regexp word1)
	   (list word1 word2))
	  (t
	   (goto-char pos1)
	   word1))))

(defun forth-smie--backward-token ()
  (forward-comment (- (point)))
  (let* ((word1 (downcase (forth-smie--backward-word)))
	 (pos1 (point))
	 (word2 (downcase (forth-smie--backward-word))))
    (cond ((string-match forth-smie--parsing-word-regexp word2)
	   (list word2 word1))
	  (t
	   (goto-char pos1)
	   word1))))

(defun forth-smie-setup ()
  (smie-setup (forth-smie--grammar) #'forth-smie--indentation-rules
	      :forward-token #'forth-smie--forward-token
	      :backward-token #'forth-smie--backward-token))

(provide 'forth-smie)

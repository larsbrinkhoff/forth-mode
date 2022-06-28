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

(defun forth-smie--forward-token ()
  (forward-comment (point-max))
  (downcase (buffer-substring-no-properties
	     (point)
	     (progn (skip-syntax-forward "w_")
		    (point)))))

(defun forth-smie--backward-token ()
  (forward-comment (- (point)))
  (downcase (buffer-substring-no-properties
	     (point)
	     (progn (skip-syntax-backward "w_")
		    (point)))))

(defun forth-smie-setup ()
  (smie-setup (forth-smie--grammar) #'forth-smie--indentation-rules
	      :forward-token #'forth-smie--forward-token
	      :backward-token #'forth-smie--backward-token))

(provide 'forth-smie)

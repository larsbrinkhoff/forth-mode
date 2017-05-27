;; SMIE based indentation

(require 'smie)

(defcustom forth-smie-basic-indent 2
  "Basic amount of indentation."
  :type 'integer
  :group 'forth-smie
  :safe 'integerp)

(defvar forth-smie--grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((control
       ("if" words "else" words "then")
       ("if" words "then")
       ("begin" words "while" words "repeat")
       ("begin" words "until")
       ("begin" words "again")
       ("?of" words "endof")
       ("of" words "endof")
       ("case" words "endcase")
       ("?do" words "loop")
       ("?do" words "+loop")
       ("do" words "loop")
       ("do" words "+loop")
       ("begin-structure" words "end-structure")
       (":" words ";")
       (":noname" words ";"))
      (words)))))

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
  (smie-setup forth-smie--grammar #'forth-smie--indentation-rules
	      :forward-token #'forth-smie--forward-token
	      :backward-token #'forth-smie--backward-token))

(provide 'forth-smie)

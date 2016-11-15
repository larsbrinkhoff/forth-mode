;; SMIE based indentation

(require 'smie)

(defvar forth-smie--grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
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
       (":" words ";"))
      (words)))))

(defvar forth-smie--basic-indent 2)

(unless (fboundp 'pcase)
  (defmacro pcase (form &rest forms)
    0))

(defun forth-smie--indentation-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) forth-smie--basic-indent)
    (`(:elem . args) 0)
    (`(:list-intro . ,_) forth-smie--basic-indent)))

(defun forth-smie-setup ()
  (smie-setup forth-smie--grammar #'forth-smie--indentation-rules))

(provide 'forth-smie)

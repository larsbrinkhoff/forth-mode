;;; forth-spec.el --- Browse words in Forth standard   -*-lexical-binding:t-*-
;;
;; Copyright (C) 2016  Helmut Eller <eller.helmut@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package makes it convenient to browse documentation for
;; standard Forth words from within Emacs.  The command
;; `forth-spec-lookup' asks for the word name and invokes the HTML
;; browser with the right URL.

;;; Code:

(eval-and-compile
  (or (require 'cl-lib nil t)
      ;; Emacs 23
      (progn
	(require 'cl)
	(defmacro cl-ecase (&rest x) `(ecase . ,x)))))

(defgroup forth-spec
  nil
  "Browsing Forth standards."
  :group 'forth)

(defcustom forth-spec-url-2012 "http://www.forth200x.org/documents/html/"
  "The URL which contains the HTML version of the standard.
If you have a local copy set this variable to
something like \"file://home/joe/docs/ANS-Forth/\".

Note: the string should have a trailing backslash."
  :type 'file
  :group 'forth-spec)

(defcustom forth-spec-url-1994 "http://lars.nocrew.org/dpans/"
  "URL for 1994 version of standard."
  :type 'file
  :group 'forth-spec)

(defcustom forth-spec-browse-url #'browse-url
  "Just in case you want to use a special browser."
  :type 'function
  :group 'forth-spec)

(defun forth-spec-lookup-2012 (name)
  "View the documentation on NAME from the Forth 2012 Standard."
  (interactive (list (forth-spec--read-name 2012)))
  (forth-spec--lookup name 2012))

(defun forth-spec-lookup-1994 (name)
  "View the documentation on NAME from the ANS'94 Forth Standard."
  (interactive (list (forth-spec--read-name 1994)))
  (forth-spec--lookup name 1994))

(defun forth-spec--lookup (name version)
  (funcall forth-spec-browse-url (forth-spec--build-url name version)))

(defvar forth-spec--lookup-history '())

(defun forth-spec--read-name (version)
  "Read a word-name in the minibuffer, with completion."
  (let ((completion-ignore-case t))
    (completing-read "Word: " (forth-spec--index version)
		     nil t (thing-at-point 'symbol)
		     'forth-spec--lookup-history)))
(eval-and-compile
  (defvar forth-spec--versioned-info
    '((2012 forth-spec-url-2012 "alpha.html" #'forth-spec--parse-2012)
      (1994 forth-spec-url-1994 "dpansf.htm" #'forth-spec--parse-1994))))

(defmacro forth-spec--versioned (name version)
  (let ((index (cl-ecase name
		 (url 1)
		 (index 2)
		 (parse-index 3))))
    `(cl-ecase ,version
       (2012 ,(elt (assoc 2012 forth-spec--versioned-info) index))
       (1994 ,(elt (assoc 1994 forth-spec--versioned-info) index)))))

(defun forth-spec--root (version)
  (forth-spec--versioned url version))

(defun forth-spec--build-url (name version)
  "Return the URL for the word NAME."
  (concat (forth-spec--root version)
	  (elt (or (assoc name (forth-spec--index version))
		   (error "Name not found in index: %s" name))
	       1)))

(defvar forth-spec--index-cache nil)

(defun forth-spec--index (version)
  "Return a list ((NAME HREF PRONUNCIATION) ...)."
  (let ((entry (assoc version forth-spec--index-cache)))
    (cond (entry (cdr entry))
	  (t
	   (let ((index (forth-spec--parse-index version)))
	     (push (cons version index) forth-spec--index-cache)
	     index)))))

(defun forth-spec--index-url (version)
  (concat (forth-spec--root version) (forth-spec--versioned index version)))

(defun forth-spec--parse-index (version)
  (forth-spec--call/url-buffer (forth-spec--index-url version)
			  (forth-spec--versioned parse-index version)))

(defun forth-spec--call/url-buffer (url fun)
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
	(with-current-buffer buffer
	  (funcall fun))
      (kill-buffer buffer))))

(defun forth-spec--parse-2012 ()
  (let ((index '())
	(case-fold-search nil)
	(rx "<td>\
<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>\
</td><td>\\(?:\"\\([^\"]+\\)\"\\)?</td>"))
    (search-forward "<table")
    (while (re-search-forward rx nil t)
      (push (list (forth-spec--decode-entities (match-string 2))
		  (match-string 1)
		  (match-string 3))
	    index))
    (reverse index)))

;; (forth-spec--parse-index 1994)
(defun forth-spec--parse-1994 ()
  (let ((index '())
	(case-fold-search nil)
	(rx "<A href=\\(dpans[^>]+\\)>[^<]+</A>[ ]*\\([^ ]+\\)[ ]*\
\\(?:<B>\\([^\<]+\\)</B>\\)?"))
    (search-forward "<PRE>")
    (while (re-search-forward rx nil t)
      (push (list (forth-spec--decode-entities (match-string 2))
		  (match-string 1)
		  (match-string 3))
	    index))
    (reverse index)))

(declare-function mm-url-decode-entities "gnus/mm-url")
(autoload 'mm-url-decode-entities "gnus/mm-url")
;; For annoying reasons, we need to declare this here.
(autoload 'mm-disable-multibyte "gnus/mm-util")

(defun forth-spec--decode-entities (string)
  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (save-match-data
      (mm-url-decode-entities))
    (buffer-string)))

(provide 'forth-spec)

;;; forth-spec.el ends here

;; forth-term.el --- Forth process in a buffer   -*- lexical-binding:t -*-

;; This is an experiment to base our process-in-a-buffer mode on
;; term-mode instead of comint-mode.  Most Forth systems switch the
;; terminal to non-canonical mode (also called "raw mode") and assume
;; that they can send ANSI escape sequences to position the cursor and
;; the like.  There are also standard words like KEY and AT-XY that
;; make much more sense if the terminal supports raw mode and
;; interprets at least some ANSI escape sequences.
;;
;; Usually, comint-mode doesn't interpret escape sequences and usually
;; doesn't support cursor positioning. (Though there seems to be some
;; support to ansi-color and ansi-osc.)  This means that comint-mode
;; will unlikely be a good Forth terminal.
;;
;; In contrast, term-mode supports cursor positioning and with
;; term-char-mode we can support "raw" input reasonably well.  There's
;; even support for an Emacs specific escape sequence "<command>\n"
;; that can be used to send commands to Emacs.  (Bash and gdb seem to
;; use this.)
;;
;; Unfortunately, term-mode has it's own set of problems.  E.g. it
;; seems to endlessly fight with Emacs to force redisplay to do
;; something that it doesn't do naturally; term-emulate-terminal calls
;; redisplay for every piece of input it receives.  term.el also
;; installs pre- and post-command-hooks in a vain attempt to keep
;; window-point at the bottom of the window; but it's obvious after
;; light testing that this doesn't work reliably.
;;
;; Also, the "" escape sequence sounds like a useful feature, but
;; again, it's not reliable.  If the <command> part is too long,
;; (probably when it is sent as two chunks) Emacs doesn't recognize
;; the sequence and simply inserts it as text.
;;
;; Given all the limitations, this code tries to be very "low tech"
;; and only provides very modest features.  In a first step, we mostly
;; try to support Gforth: parse error messages and crude wrappers
;; around Gforth's EDIT, BROWSE and WHERE commands.  (Those commands
;; actually invoke emacsclient as the "return channel".)

(require 'term)
(require 'cl-lib)
(require 'forth-interaction-mode) ; for forth-implementation-matches

(defvar forth-term--buffer-name "*forth-term*")
(defvar forth-term--command nil)

(defun forth-term--buffer (&optional create)
  (funcall (if create #'get-buffer-create #'get-buffer)
	   forth-term--buffer-name))

(defun forth-term--process ()
  (let* ((buf (forth-term--buffer))
	 (proc (and buf (get-buffer-process buf))))
    (cl-assert proc)
    proc))

(defun forth-term--command-function (string)
  (cond ((string-match "^pong \\(.*\\)$" string)
	 (message "PONG: %s" (match-string 1 string)))
	(t
	 (error "Unsupported eterm command: %S" string))))

(defun forth-term--init-backend ()
  (let* ((proc (get-buffer-process (current-buffer))))
    ;; Wait until we receive at least one line of output
    (while (< (car (buffer-line-statistics)) 1)
      (accept-process-output proc 1))
    (let* ((banner (buffer-string))
	   (probe (cl-assoc-if (lambda (pattern)
				 (string-match pattern banner))
			       forth-implementation-matches)))
      (cond (probe
	     (let ((impl (cdr probe)))
	       (setq-local forth-implementation impl)
	       (let ((load-path (cons forth-backend-dir load-path)))
		 (require impl))
	       (run-hook-with-args 'forth-term-init-backend-hook impl proc)))
	    (t
	     (display-warning
	      'forth-term
	      (format "Unknown Forth implementation\nBanner: %S" banner)))))))

(defun forth-term--start-process (buffer)
  (with-current-buffer buffer
    (let* ((cmd (read-shell-command "Forth program: "
				    (car shell-command-history)))
	   (args (split-string-shell-command cmd))
	   (_ (progn
		(let ((inhibit-read-only t))
		  (erase-buffer))
		(term-mode)))
	   (procname (buffer-name buffer))
	   (buf (term-exec buffer procname (car args) nil (cdr args)))
	   (proc (get-buffer-process buf)))
      (forth-term--init-backend)
      (setq-local term-command-function #'forth-term--command-function)
      (term-char-mode)
      proc)))

(defun forth-term ()
  "Start an interactive forth session."
  (interactive)
  (let* ((buffer (forth-term--buffer t)))
    (when (not (term-check-proc buffer))
      (forth-term--start-process buffer))
    (pop-to-buffer-same-window buffer)))

(defun forth-term--prepare-next-error-function ()
  (with-current-buffer (forth-term--buffer)
    (setq next-error-last-buffer (current-buffer))
    (setq next-error-function #'compilation-next-error-function)
    (setq-local compilation-locs
		(make-hash-table :test 'equal :weakness 'value))
    (setq-local compilation-messages-start (point))
    (setq-local compilation-current-error nil)))

;; This is for testing <command>\n as escape sequence to send
;; commands to the Forth process.  Only implemented for Gforth so far.
(defun forth-term-ping (string)
  (interactive "sPing: ")
  (cl-assert (not (string-match "\n" string)))
  (term-send-string (forth-term--process) (concat "ping " string "\n")))

(defun forth-term-include ()
  "Load (i.e. INCLUDE) the file for the current buffer."
  (interactive)
  (save-some-buffers)
  (forth-term--prepare-next-error-function)
  (term-proc-query (forth-term--process)
		   (format "s\" %s\" included\n" (buffer-file-name))))

(defun forth-term-edit (name)
  "Jump to the source code of NAME."
  (interactive (list (read-from-minibuffer "Definition name: "
					   (thing-at-point 'symbol))))
  (term-proc-query (forth-term--process) (format "edit %s\n" name)))

(defun forth-term-browse (pattern)
  "List words matching PATTERN.
You can use `next-error' to jump to the individual words in turn."
  (interactive (list (read-from-minibuffer "Pattern: "
					   (thing-at-point 'symbol))))
  (forth-term--prepare-next-error-function)
  (term-proc-query (forth-term--process) (format "browse %s\n" pattern)))

(defun forth-term-where (name)
  "List the locations where the word NAME is used.
You can use `next-error' to jump to cycle through the listed
locations."
  (interactive (list (read-from-minibuffer "Definition name: "
					   (thing-at-point 'symbol))))
  (forth-term--prepare-next-error-function)
  (term-proc-query (forth-term--process) (format "where %s\n" name)))

(provide 'forth-term)

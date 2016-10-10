(require 'forth-interaction-mode)

(setq forth-introspection-p t)
(forth-interaction-send (concat "include " forth-backend-dir "/lbforth.fth"))

(defun forth-end-of-line (buffer pos)
  (save-excursion
    (with-current-buffer buffer
      (goto-char pos)
      (end-of-line)
      (- (point) pos))))

(defun forth-previous-word (buffer pos)
  (save-excursion
    (with-current-buffer buffer
      (goto-char pos)
      (re-search-backward "[[:graph:]]")
      (1+ (point)))))

(defun forth-props (buffer start end depth)
  (add-text-properties start (forth-previous-word buffer end)
		       `(mouse-face highlight
			 help-echo ,(format "Stack Depth: %d" depth))))

(defun forth-introspect (string buffer start)
  (let ((list (split-string string))
	(end (forth-end-of-line buffer start)))
    (set-text-properties start end nil)
    (setf (car (last list 2)) (prin1-to-string end))
    (loop for (pos depth next) on list by #'cddr
          when next do (forth-props buffer
				    (+ start (read pos))
				    (+ start (read next))
				    (read depth)))))

(provide 'lbforth)

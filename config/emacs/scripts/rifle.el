(defun rifle-run ()
  "Runs the 'rifle-run' command on a popup, via a background buffer."
  (interactive)
  (let (filetype (get-filetype)
	filename (buffer-file-name))
    (if (or (eq filetype nil)
	    (eq filename nil))
	(progn
	  (message "(rifle) failed to get filetype or filename")
	  (message filetype)
	  (message filename))
      (progn
	(message filetype)
	(message filename)))))

(defun get-filetype ()
  "Gets the filetype of the current buffer."
  (cond ((eq major-mode 'rust-mode) "rust")
	(t nil)))

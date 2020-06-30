(defun rifle-run ()
  "Runs the 'rifle-run' command on a popup, via a background buffer."
  (interactive)
  ;; (defvar filename (buffer-file-name))
  (start-process "rifle" "rifle"
		 "termup" "runread" "rifle-run"
		 "run"
		 (pcase major-mode
		   ('rust-mode "rust")
		   (major-mode (error (concat "invalid filetype: " (symbol-name major-mode)))))
		 (buffer-file-name)))

(defun ice-config-reload ()
  "Reload most of the config."
  (interactive)
  (load-file (f-join user-emacs-directory "init.el")))

(defun file-upwards-parent (file &optional starting-directory)
  "Recursively checks for the existence of `file' in `starting-directory' and its parents, returning either the parent where the file was found or nil if no file was found.
`starting-directory' defaults to \".\""
  (f-traverse-upwards (lambda (path)
			(f-exists? (f-expand file path)))
		      (or starting-directory ".")))

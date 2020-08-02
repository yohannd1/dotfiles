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

(defun ice-get-xres (resource fallback)
  "Attempts to get an X resource, falling back to `FALLBACK' if any error occurs.
On non-linux platforms `FALLBACK' is always returned."
  (if (and IS-LINUX (display-graphic-p))
      (or (x-get-resource resource "") fallback)
    fallback))

(defun ice-escape ()
  "Aborts current functions."
  ;; (from Doom Emacs)
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

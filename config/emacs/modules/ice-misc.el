;; There's a binary I found called `xgetres'. It helps with getting X
;; resources from the command line. If I can find it here it'd help a
;; lot, since emacs refuses to reload the xrdb at runtime.
(defconst ice--xgetres-path (executable-find "xgetres"))

(defun file-upwards-parent (file &optional starting-directory)
  "Recursively checks for the existence of `file' in `starting-directory' and its parents, returning either the parent where the file was found or nil if no file was found.
`starting-directory' defaults to \".\""
  (f-traverse-upwards (lambda (path)
                        (f-exists? (f-expand file path)))
                      (or starting-directory ".")))

(defun get-xres (resource fallback)
  "Attempts to get an X resource, falling back to `FALLBACK' if any error occurs.
On non-linux platforms `FALLBACK' is always returned."
  (if (and IS-LINUX (getenv "DISPLAY"))
      (let ((result (if ice--xgetres-path
                         (string-trim (shell-command-to-string (concat "xgetres Emacs." resource)))
                       (x-get-resource resource ""))))
        (pcase result
          ("" fallback)
          ('nil fallback)
          (other other)))
    fallback))

;; (from Doom Emacs)
(defun ice-escape ()
  "Attempts to abort the command being currently executed."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((keyboard-quit))))

(defun ice-tty-change-cursor (&optional code)
  "Sends ANSI escape codes to the TTY, indicating it to change the cursor."
  (unless (display-graphic-p)
    (send-string-to-terminal (concat "\033["
                                     (number-to-string (or code 2))
                                     " q"))))

(defmacro inline-hook! (hook-name hook-arg-list &rest body)
  `(add-hook ,hook-name (lambda ,hook-arg-list ,@body)))


(provide 'ice-misc)

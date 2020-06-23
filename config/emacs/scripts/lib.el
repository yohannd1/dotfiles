(defun rl () "Reloads the init.el file at runtime"
       (interactive)
       (load-file "~/.emacs.d/init.el"))

(defun change-cursor (&optional code)
  "Sends an escape code to the terminal, indicating that the cursor should change."
  (if (eq code nil) (setq code 2))
  (unless (display-graphic-p)
    (send-string-to-terminal (concat "\033["
				     (number-to-string code)
				     " q"))))

(defvar theme-current/gui nil
  "The theme to be used on graphical mode.")

(defvar theme-current/term nil
  "The theme to be used on terminals.")

(defun theme-update ()
  "Detects whether the current frame is graphical or on a terminal and then loads the corresponding theme to it."
  (if (display-graphic-p)
      (progn
	(load-theme theme-current/gui t)
	(set-frame-font current-font nil t))
      (progn
	(load-theme theme-current/term t))))

(defun save-buffer-reload-term-dash ()
  (interactive)
  (save-buffer)
  (load-theme 'term-dash t))

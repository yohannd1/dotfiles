(defun ice-tty-change-cursor (&optional code)
  "Sends ANSI escape codes to the TTY, indicating it to change the cursor."
  (unless (display-graphic-p)
    (send-string-to-terminal (concat "\033["
                                     (number-to-string (or code 2))
				                     " q"))))

;; Set up package manager
(when at-startup
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(defun config-reload ()
  "Reloads most of the config at runtime."
  (interactive)
  (load-file (concat emacs-folder "/init.el")))

(defun terminal-change-cursor (&optional code)
  "Sends ANSI cursor changing escape codes to the terminal."
  (unless (display-graphic-p)
    (send-string-to-terminal (concat "\033["
				     (number-to-string (or code 2))
				     " q"))))

(defun theme-update ()
  "Detects whether the current frame is graphical or on a terminal and then loads the corresponding theme to it."
  (if (display-graphic-p)
      (progn
	(when current-theme-gui
	  (load-theme current-theme-gui))
	(when current-font-gui
	  (set-frame-font current-font-gui)))
    (progn
      (when current-theme-tty
	(load-theme current-theme-tty t)))))

(defun rifle-run ()
  "Runs the 'rifle-run' command on a popup, via a background buffer."
  (interactive)
  (let ((mode (replace-regexp-in-string "-mode$" ""
					(symbol-name-p major-mode))))
    (start-process "rifle" "Rifle Background Process"
		   (or (getenv "TERMINAL") "xterm") "-e" "runread" "rifle-run" "run"
		   (pcase mode
		     ;; Insert extra patterns here
		     (mode mode)))))

(defvar current-theme-gui nil
  "The theme to be used on graphical mode.")

(defvar current-theme-tty nil
  "The theme to be used on terminals.")

(defvar current-font-gui nil
  "The font used on graphical mode.")

;; TODO: refactor this to load multiple libraries with different functions

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

(defun core/load-theme ()
  (interactive)
  (load-theme current-theme t)
  (when (display-graphic-p)
    (set-frame-font current-font)))

(defun rifle-run ()
  "Runs the 'rifle-run' command on a popup, via a background buffer."
  (interactive)
  (let ((mode (replace-regexp-in-string "-mode$" ""
					(symbol-name major-mode))))
    (start-process "rifle" "Rifle Background Process"
		   (or (getenv "TERMINAL") "xterm") "-e" "runread" "rifle-run" "run"
		   (pcase mode
		     ;; Insert extra patterns here
		     (mode mode)))))

(defvar current-theme 'wombat
  "The theme to load with core/load-theme")

(defvar current-font "monospace 10"
  "The font to load if the GUI is available")

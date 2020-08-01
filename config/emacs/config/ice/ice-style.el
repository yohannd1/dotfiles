(defvar ice-style-theme 'wombat
  "The theme to load on startup.")

(defvar ice-style-font "monospace 10"
  "The font to load on startup.")

(defun ice--style-update ()
  (interactive)
  (load-theme ice-style-theme t)
  (when (display-graphic-p)
    (set-frame-font ice-style-font)))

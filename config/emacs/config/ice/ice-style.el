(defvar ice-style-theme 'wombat
  "The theme to load on startup.")

(defvar ice-style-font-family "monospace"
  "The family of the main font, that should be used (almost) everywhere on emacs.")

(defvar ice-style-font-height 100
  "The height of the main font.")

(defun ice-style-update ()
  (interactive)
  (run-hooks 'ice-style-update-hook)
  (when ice-style-theme
    (load-theme ice-style-theme t))
  ;; Update face - main font
  (dolist (face (list 'default))
    (set-face-attribute face nil
                        :family ice-style-font-family
                        :height ice-style-font-height))
  ;; Update face - tabs
  (dolist (face (list 'centaur-tabs-default
                      'centaur-tabs-selected
                      'centaur-tabs-unselected
                      'centaur-tabs-selected-modified
                      'centaur-tabs-unselected-modified
                      'centaur-tabs-active-bar-face
                      'centaur-tabs-modified-marker-selected
                      'centaur-tabs-modified-marker-unselected))
    (set-face-attribute face nil
                        :family ice-style-font-family
                        :height 90)))

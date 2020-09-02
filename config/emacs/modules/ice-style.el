;; -*- lexical-binding: t; -*-

(defvar ice-style-current-theme 'wombat
  "The theme to load on startup.")

(defvar ice-style-font-family "monospace"
  "The family of the main font, that should be used (almost) everywhere on emacs.")

(defvar ice-style-font-height 100
  "The height of the main font.")

(defvar ice-style-before-update-hook '()
  "A hook that is ran before the main part of `ice-style-update' is ran.")

(defvar ice-style-before-after-hook '()
  "A hook that is after before the main part of `ice-style-update' is ran.")

(defun ice-style-update ()
  (interactive)

  ;; "before" hook
  (run-hooks 'ice-style-before-update-hook)

  ;; Load the theme
  (when ice-style-current-theme
    (load-theme ice-style-current-theme t))

  ;; Update face - main font
  (dolist (face (list 'default))
    (set-face-attribute face nil
                        :family ice-style-font-family
                        :height ice-style-font-height))

  ;; Update face - centaur-tabs
  (when (featurep 'centaur-tabs)
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

  ;; "after" hook
  (run-hooks 'ice-style-after-update-hook))

(provide 'ice-style)

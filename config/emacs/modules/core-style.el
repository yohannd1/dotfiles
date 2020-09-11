;;; -*- lexical-binding: t; -*-

(defvar core-style-current-theme 'wombat
  "The theme to load on startup.")

(defvar core-style-font-family "monospace"
  "The family of the main font, that should be used (almost) everywhere on emacs.")

(defvar core-style-font-height 100
  "The height of the main font.")

(defvar core-style-before-update-hook nil
  "A hook that is ran before the main part of `core-style-update' is ran.")

(defvar core-style-before-after-hook nil
  "A hook that is after before the main part of `core-style-update' is ran.")

(defun core-style-update ()
  (interactive)

  ;; "before" hook
  (run-hooks 'core-style-before-update-hook)

  ;; Load the theme
  (when core-style-current-theme
    (load-theme core-style-current-theme t))

  ;; Update face - main font
  (dolist (face (list 'default))
    (set-face-attribute face nil
                        :family core-style-font-family
                        :height core-style-font-height))

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
                            :family core-style-font-family
                            :height 90)))

  ;; "after" hook
  (run-hooks 'core-style-after-update-hook))

(provide 'core-style)

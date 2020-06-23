(custom-set-variables
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" default)))
 '(safe-local-variable-values (quote ((origami-fold-style . triple-braces)))))
(custom-set-faces)

(when (boundp 'at-startup)
    (setq at-startup nil))

(defvar at-startup t
  "Whether the current execution is at startup.")

(when at-startup
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/scripts"))

(load "lib.el")
(load "packages.el")
(load "config.el")

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (scroll-bar-mode 0))

(when at-startup
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame) (with-selected-frame frame (theme-update))))
    (theme-update))

  (unless (display-graphic-p)
    (add-hook 'evil-normal-state-entry-hook #'change-cursor)
    (add-hook 'evil-motion-state-entry-hook #'change-cursor)
    (add-hook 'evil-replace-state-entry-hook #'change-cursor)
    (add-hook 'evil-visual-state-entry-hook #'change-cursor)
    (add-hook 'evil-insert-state-entry-hook (lambda () (change-cursor 6)))
    (add-hook 'evil-operator-state-entry-hook (lambda () (change-cursor 3)))
    (add-hook 'evil-emacs-state-entry-hook #'change-cursor)))

;; Set startup variable to false if it has been defined previously
(when (boundp 'at-startup)
  (setq at-startup nil))

;; Set startup variable to true at first call
(defvar at-startup t
  "Whether the config file is being ran for the first time in the session.")

;; Get the config folder path
(defvar emacs-folder
  (file-name-directory (or load-file-name buffer-file-name))
  "The place where all the config files are stored.")

;; Add places to the load path
(when at-startup
  (add-to-list 'custom-theme-load-path (concat emacs-folder "/themes"))
  (add-to-list 'load-path (concat emacs-folder "/elisp")))

;; Load core and main configuration
(load "core.el")
(load "main.el")

;; Had to place this here - before the theme loading - for "safe theme" signature issues
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default)))
 '(package-selected-packages
   (quote
    (centaur-tabs linum-relative which-key use-package typescript-mode try rust-mode rainbow-delimiters origami markdown-mode lua-mode julia-mode helm haskell-mode evil-commentary csharp-mode clojure-mode base16-theme auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Some messy hook code
(when at-startup
  (if (daemonp)
      (progn
	(add-hook 'after-make-frame-functions
		  (lambda (frame)
		    (with-selected-frame frame (theme-update))))
	(add-hook 'after-make-frame-functions
		  (lambda (_)
		    (unless (display-graphic-p)
		      (define-key evil-motion-state-map (kbd "ç") #'evil-ex)))))
    (theme-update))

  (unless (display-graphic-p)
    ;; Cursor changing code for terminal
    (add-hook 'evil-normal-state-entry-hook #'change-cursor)
    (add-hook 'evil-motion-state-entry-hook #'change-cursor)
    (add-hook 'evil-replace-state-entry-hook #'change-cursor)
    (add-hook 'evil-visual-state-entry-hook #'change-cursor)
    (add-hook 'evil-insert-state-entry-hook (lambda () (change-cursor 6)))
    (add-hook 'evil-operator-state-entry-hook (lambda () (change-cursor 3)))
    (add-hook 'evil-emacs-state-entry-hook #'change-cursor)))

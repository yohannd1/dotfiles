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
(load "rifle.el")

(global-visual-line-mode 1)
(global-linum-mode 1)
(electric-pair-mode 1)

(unless (display-graphic-p)
  (xterm-mouse-mode))

(setq inhibit-startup-message t)
(setq vc-follow-symlinks nil)
(setq linum-format "%3d ")

;; Backup and autosave files
(setq version-control t
      kept-new-versions 5
      kept-old-versions 3
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.cache/emacs/backup"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves" t)))

(setq c-default-style "linux"
      c-basic-offset 4)

(setq scroll-step 1
      scroll-margin 5)

(setq theme-current/gui 'base16-tomorrow-night)
(setq theme-current/term 'term-dash)
(setq current-font "JetBrains Mono Medium 11")

(evil-set-initial-state 'term-mode 'emacs) ; emacs bindings for term-mode

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (scroll-bar-mode 0))

;; Bindings
(defvar evil-leader-map (make-sparse-keymap))
(define-key evil-normal-state-map (kbd "SPC") evil-leader-map)
(define-key evil-leader-map "r" #'rifle-run)

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

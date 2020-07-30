;; Require the "use-package" package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; TODO: stop using this (make my own version of the theme)
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-default-dark t))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "ç") #'evil-ex)
    (define-key evil-motion-state-map (kbd "M-j") #'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-k") #'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-l") #'evil-window-right)
    (define-key evil-motion-state-map (kbd "M-h") #'evil-window-left)
    (define-key evil-insert-state-map (kbd "C-y") #'evil-paste-after)

    (defvar evil-leader-map (make-sparse-keymap))
    (define-key evil-normal-state-map (kbd "SPC") evil-leader-map)
    (define-key evil-leader-map "rr" #'rifle-run)
    (define-key evil-leader-map "ed" #'dired-emacs-folder)))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

;; (use-package origami
;;   :ensure t
;;   :config
;;   (global-origami-mode 1)
;;   (defun nin-origami-toggle-node ()
;;     (interactive)
;;     (save-excursion ; leave point where it is
;;       (goto-char (point-at-eol)) ; then go to the end of line
;;       (origami-toggle-node (current-buffer) (point)))) ; and try to fold
;;   (define-key evil-normal-state-map (kbd "TAB") 'nin-origami-toggle-node)
;;   (define-key evil-normal-state-map (kbd "<backtab>") 'origami-close-all-nodes))

(use-package rust-mode
  :ensure t)

(use-package haskell-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package clojure-mode
  :ensure t)

(use-package julia-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'rainbow-delimiters-mode-enable))

(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode))

(use-package helm
  :ensure t)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package linum-relative
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'linum-relative-mode))

(use-package centaur-tabs
  :ensure t
  :config
  ;; Options
  ;; (setq centaur-tabs-set-bar 'over) 
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  (defun centaur-tabs-buffer-groups () '("Emacs"))
  ;; Enable
  (centaur-tabs-mode t)
  ;; Bindings
  (define-key global-map (kbd "C-j") #'centaur-tabs-forward)
  (define-key global-map (kbd "C-k") #'centaur-tabs-backward)
  (define-key evil-motion-state-map (kbd "C-j") #'centaur-tabs-forward)
  (define-key evil-motion-state-map (kbd "C-k") #'centaur-tabs-backward))

(defun dired-emacs-folder ()
  (interactive)
  (dired emacs-folder))

;; Backup and autosave files
(setq version-control t
      kept-new-versions 5
      kept-old-versions 3
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist '(("" . "~/.cache/emacs/backup"))
      auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves" t)))

;; Coding styles
(setq c-default-style "linux"
      c-basic-offset 4)

(setq confirm-nonexistent-file-or-buffer nil
      frame-resize-pixelwise t
      window-resize-pixelwise t
      inhibit-startup-message t
      vc-follow-symlinks nil
      linum-format "%3d "
      scroll-step 1
      scroll-margin 5)

;; Minor mode changes
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-visual-line-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode 0)) ;; Disable scroll bar only in graphical mode because it doesn't even exist in terminal mode
  (progn
    (xterm-mouse-mode))) ;; Enable mouse support in terminal mode

;; Configuration
(setq current-theme-gui 'base16-tomorrow-night
      current-theme-tty 'term-dash
      current-font-gui "JetBrains Mono Medium 10")


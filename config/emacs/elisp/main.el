;; Require the "use-package" package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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
    (define-key evil-leader-map "rr" #'(lambda ()
					 (interactive)
					 (rifle-run)))
    (define-key evil-leader-map "de" #'(lambda ()
					 (interactive)
					 (dired emacs-folder)))
    (define-key evil-leader-map "cr" #'config-reload)
    (define-key evil-leader-map "ce" #'(lambda ()
					 (interactive)
					 (find-file (concat emacs-folder "/elisp/main.el"))))

    (which-key-add-key-based-replacements "SPC r" "rifle commands")
    (which-key-add-key-based-replacements "SPC rr" "rifle run")
    (which-key-add-key-based-replacements "SPC d" "dired aliases")
    (which-key-add-key-based-replacements "SPC de" "dired - emacs folder")
    (which-key-add-key-based-replacements "SPC c" "config commands")
    (which-key-add-key-based-replacements "SPC cr" "reload config")
    (which-key-add-key-based-replacements "SPC ce" "edit config - main.el")))

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

(use-package rust-mode ;; TODO: autoload
  :ensure t)

(use-package haskell-mode ;; TODO: autoload
  :ensure t)

(use-package markdown-mode ;; TODO: autoload
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package clojure-mode ;; TODO: autoload
  :ensure t)

(use-package julia-mode ;; TODO: autoload
  :ensure t)

(use-package csharp-mode ;; TODO: autoload
  :ensure t)

(use-package typescript-mode ;; TODO: autoload
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'rainbow-delimiters-mode-enable))

(use-package auto-complete ;; TODO: setup
  :ensure t
  :config
  (global-auto-complete-mode))

(use-package helm ;; TODO: setup
  :ensure t)

(use-package try
  :ensure t)

(use-package which-key ;; TODO: setup
  :ensure t
  :config
  (which-key-mode))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-format "%3s ")
  (add-hook 'after-change-major-mode-hook #'linum-relative-mode)
  (add-hook 'minibuffer-setup-hook #'linum-relative-off))

(use-package centaur-tabs ;; TODO: cleanup
  :ensure t
  :config
  (setq centaur-tabs-style "bar"
	;; centaur-tabs-set-bar 'under
	;; x-underline-at-descent-line t
	centaur-tabs-set-close-button nil
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "+"
	centaur-tabs-adjust-buffer-order t
	centaur-tabs-set-icons t)
  (centaur-tabs-enable-buffer-reordering)
  ;; TODO: tab titles
  (defun centaur-tabs-buffer-groups ()
    (list
     (cond
      ((derived-mode-p 'dired-mode) "Dired")
      (t "Main"))))
  (defun centaur-tabs-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p "*tramp" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*Completions" name)
       (string-prefix-p "*Backtrace*" name)

       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
	    (not (file-name-extension name))))))
  (centaur-tabs-mode t)
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
      vc-follow-symlinks nil)

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
(setq current-theme 'term-dash
      current-font "JetBrains Mono Medium 10")

;; Fix scrolling (stolen from Doom Emacs)
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

;; Title
(setq frame-title-format '("%b – Emacs")
      icon-title-format frame-title-format)

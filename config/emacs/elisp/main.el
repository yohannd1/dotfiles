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
    (define-key evil-leader-map "dh" #'(lambda ()
					 (interactive)
					 (dired "~")))
    (define-key evil-leader-map "cr" #'config-reload)
    (define-key evil-leader-map "ce" #'(lambda ()
					 (interactive)
					 (find-file (concat emacs-folder "/elisp/main.el"))))
    (define-key evil-leader-map "d." #'(lambda ()
					 (interactive)
					 (dired ".")))
    (define-key evil-leader-map "." #'helm-find-files)

    (which-key-add-key-based-replacements "SPC r" "rifle commands")
    (which-key-add-key-based-replacements "SPC rr" "rifle run")
    (which-key-add-key-based-replacements "SPC d" "dired aliases")
    (which-key-add-key-based-replacements "SPC d." "dired - current folder")
    (which-key-add-key-based-replacements "SPC de" "dired - emacs folder")
    (which-key-add-key-based-replacements "SPC dh" "dired - home folder")
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

(use-package esup
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t)

(use-package csharp-mode
  :ensure t
  :defer t)

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'rainbow-delimiters-mode-enable))

(use-package auto-complete ;; TODO: setup
  :ensure t
  :config
  (global-auto-complete-mode))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") #'helm-M-x)
  (setq helm-M-x-fuzzy-match t
	helm-buffers-fuzzy-matching t))

(use-package try
  :ensure t
  :defer t)

(use-package which-key ;; TODO: setup
  :ensure t
  :config
  (which-key-mode))

(use-package format-all
  :ensure t
  :config
  (define-key evil-leader-map "f" #'format-all-buffer))

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
	centaur-tabs-set-close-button nil
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "+"
	centaur-tabs-adjust-buffer-order t
	centaur-tabs-set-icons t)
  (centaur-tabs-headline-match)
  (centaur-tabs-enable-buffer-reordering)
  ;; TODO: tab category titles
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
       (string-prefix-p "*Command Line*" name)
       (string-prefix-p "*eldoc for" name)

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
(global-visual-line-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)

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

;; Use ESC for cancelling commands
(if (display-graphic-p) ;; on isearch
    (define-key isearch-mode-map [escape] 'isearch-abort)
  (define-key isearch-mode-map "\e" 'isearch-abort))
(global-set-key [escape] 'keyboard-escape-quit) ;; everywhere else

(unless (assq 'menu-bar-lines default-frame-alist)
  ;; We do this in early-init.el too, but in case the user is on Emacs 26 we do
  ;; it here too: disable tool and scrollbars, as Doom encourages
  ;; keyboard-centric workflows, so these are just clutter (the scrollbar also
  ;; impacts performance).
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; These are disabled directly through their frame parameters, to avoid the
;; extra work their minor modes do, but we have to unset these variables
;; ourselves, otherwise users will have to cycle them twice to re-enable them.
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Always avoid GUI
(setq use-dialog-box nil)

;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Show current key-sequence in minibuffer ala 'set showcmd' in vim. Any
;; feedback after typing is better UX than no feedback at all.
(setq echo-keystrokes 0.02)

;; Don't prompt for confirmation when we create a new file or buffer (assume the
;; user knows what they're doing).
(setq confirm-nonexistent-file-or-buffer nil)

;; Stop blinking things
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; Buffer name styles
(setq uniquify-buffer-name-style 'forward)

;; Prevent blinking
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Replace yes/no with y/n
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Some terminals offer two different cursors: a “visible” static cursor and a
;; “very visible” blinking one. By default, Emacs uses the very visible cursor
;; and switches to it when you start or resume Emacs. If `visible-cursor' is nil
;; when Emacs starts or resumes, it uses the normal cursor.
(setq visible-cursor nil)

;; Allow for minibuffer-ception. Sometimes we need another minibuffer command
;; while we're in the minibuffer.
(setq enable-recursive-minibuffers t)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

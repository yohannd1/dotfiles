;; Main configuration file
;;
;; This file is pretty much the "central point" of my config.

;; So, you ask yourself why there are some modules with the "ice" prefix.
;; Well, it's just a made up library name. It's short and helps me
;; distinguish it from other (external or builtin) packages.
(require 'ice-essentials)
(require 'ice-style)
(require 'ice-misc)
(require 'ice-rifle)

(use-package bind-map
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  :config
  (setq-default evil-symbol-word-search t) ;; make evil-search-word look for symbol rather than word boundaries
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-mode 1))

(use-package auto-package-update
  :ensure t)

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-magit
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package esup
  :ensure t
  :defer t)

(use-package lua-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (inline-hook! 'markdown-mode-hook ()
                (auto-fill-mode)))

(use-package git-commit-message
  :ensure nil
  :defer t)

(use-package clojure-mode
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package csharp-mode
  :ensure t
  :defer t)

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package racket-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'after-change-major-mode-hook #'rainbow-delimiters-mode-enable))

(use-package auto-complete
  :ensure t
  :config
  (setq ac-auto-start 2
        ac-auto-show-menu t
        ac-use-menu-map t)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil)
  (inline-hook! 'rust-mode-hook () (auto-complete-mode 1)))

;; (use-package company
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook #'global-company-mode))

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories nil) ;; default value: '("../" "./")
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "C-u") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "C-m") #'ivy-alt-done))

(use-package counsel
  :ensure t
  :after ivy
  :config
  (global-set-key (kbd "M-x") #'counsel-M-x))

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode))

(use-package try
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package format-all
  :ensure t
  :defer t)

;; (use-package doom-modeline
;;   :ensure t
;;   :init
;;   (doom-modeline-mode 1))

(use-package spaceline
  :ensure t
  :config
  (spaceline-emacs-theme)
  (spaceline-define-segment tty-or-gui
    (if (display-graphic-p)
        "gui"
      "tty"))
  (spaceline-compile
    ;; Left Side
    '((anzu
       :priority 100)
      (evil-state
       :face highlight-face
       :priority 100)
      (tty-or-gui
       :face other-face
       :when active
       :priority 100)
      (buffer-id
       :face default-face)
      (major-mode
       :face other-face
       :priority 90)
      ((buffer-modified buffer-size)
       :face default-face
       :priority 80))

    ;; Right Side
    '((selection-info
       :face default-face
       :priority 95)
      (buffer-encoding-abbrev
       :face default-face
       :priority 96)
      ((point-position line-column)
       :face other-face
       :priority 96)
      (buffer-position
       :face highlight-face
       :priority 99))))

(use-package origami
  :ensure t
  :config
  (with-eval-after-load 'evil
    (define-key evil-motion-state-map "zo" #'origami-open-node)
    (define-key evil-motion-state-map "zc" #'origami-close-node)
    (define-key evil-motion-state-map "za" #'origami-toggle-node)))

;; https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-dired.el
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alGhvF")
  (inline-hook! 'dired-mode-hook ()
                (let ((name (buffer-name)))
                  (rename-buffer (concat "*Dired: " (replace-regexp-in-string "/$" "" name) "*")))
                (toggle-truncate-lines 1)))

(use-package xclip
  ;; Clipboard support in the terminal.
  ;; Works only if the `xclip' binary is installed.
  :ensure t
  :config
  (xclip-mode 1))

;; (use-package centaur-tabs ;; TODO: move & remake
;;   :ensure t
;;   :config
;;   (add-hook 'ice-style-after-update-hook #'centaur-tabs-headline-match)
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-set-close-button nil
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-modified-marker "+"
;;         centaur-tabs-adjust-buffer-order t
;;         centaur-tabs-set-icons nil)
;;   ;; TODO: tab category titles
;;   (defun centaur-tabs-buffer-groups ()
;;     (list
;;      (cond
;;       ((derived-mode-p 'dired-mode) "Dired")
;;       (t "Main"))))
;;   (defun centaur-tabs-hide-tab (x)
;;     (let ((name (format "%s" x)))
;;       (or
;;        ;; Current window is not dedicated window.
;;        (window-dedicated-p (selected-window))

;;        ;; Buffer name not match below blacklist.
;;        (string-prefix-p "*epc" name)
;;        (string-prefix-p "*helm" name)
;;        (string-prefix-p "*Helm" name)
;;        (string-prefix-p "*Compile-Log*" name)
;;        (string-prefix-p "*lsp" name)
;;        (string-prefix-p "*company" name)
;;        (string-prefix-p "*Flycheck" name)
;;        (string-prefix-p "*tramp" name)
;;        (string-prefix-p " *Mini" name)
;;        (string-prefix-p "*help" name)
;;        (string-prefix-p "*straight" name)
;;        (string-prefix-p " *temp" name)
;;        (string-prefix-p "*Help" name)
;;        (string-prefix-p "*Completions" name)
;;        (string-prefix-p "*Backtrace*" name)
;;        (string-prefix-p "*Command Line*" name)
;;        (string-prefix-p "*eldoc for" name)

;;        ;; Is not magit buffer.
;;        (and (string-prefix-p "magit" name)
;;             (not (file-name-extension name))))))
;;   (centaur-tabs-mode t)
;;   (centaur-tabs-enable-buffer-reordering))

;; (use-package edwina
;;   :ensure t
;;   :config
;;   (setq display-buffer-base-action '(display-buffer-below-selected))
;;   (edwina-setup-dwm-keys)
;;   (edwina-mode 1))

;; Set options for ice-style.
(inline-hook! 'ice-style-before-update-hook ()
              (setq ice-style-current-theme 'base16
                    ice-style-font-family (get-xres "font" ice-style-font-family)
                    ice-style-font-height 100))

;; Backup / autosave files
;; TODO: disable backup & autosave altogether if on "foreign" machines.
(setq version-control t
      kept-new-versions 5
      kept-old-versions 3
      delete-old-versions t
      backup-by-copying t
      vc-make-backup-files t
      backup-directory-alist `(("" . ,(f-join user-cache-directory "backups")))
      auto-save-file-name-transforms `((".*" ,(f-join user-cache-directory "saves") t)))

;; c-mode
(inline-hook! 'c-mode-hook ()
              (c-set-style "linux")
              (setq indent-tabs-mode t))

;; sh-mode
(setq sh-basic-offset 2)

;; c++-mode
(c-add-style "c++"
             '("stroustrup"
               (indent-tabs-mode . nil)                          ;; use spaces rather than tabs
               (c-basic-offset . 4)                              ;; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)             ;; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))
(inline-hook! 'c++-mode-hook ()
              (c-set-style "c++"))

;; asm-mode
(inline-hook! 'asm-mode ()
              (setq indent-tabs-mode t))

;; Automatically create a file/buffer when called if it doesn't exist
(setq confirm-nonexistent-file-or-buffer nil)

;; Pixelwise resizing
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

;; No standard startup buffer
(setq inhibit-startup-message t)

;; Don't follow symlinks when opening files
(setq vc-follow-symlinks nil)

;; Soft wrap
(global-visual-line-mode 1)

;; Disable the warning "X and Y are the same file". It's fine to ignore this
;; warning as it will redirect you to the existing buffer anyway.
;; (from Doom Emacs)
(setq find-file-suppress-same-file-warnings t)

;; The POSIX standard defines a line is "a sequence of zero or more non-newline
;; characters followed by a terminating newline", so files should end in a
;; newline. Windows doesn't respect this (because it's Windows), but we should,
;; since programmers' tools tend to be POSIX compliant.
;; (from Doom Emacs)
(setq require-final-newline t)

;; Relative line numbers
(setq display-line-numbers-type 'relative
      display-line-numbers-grow-only t
      display-line-numbers-width-start 3)
(global-display-line-numbers-mode 1)

;; Electric pairs - automatically close brackets, quotes etc.
(electric-pair-mode 1)

;; Show matching pairs
(show-paren-mode 1)

;; Fix scrolling problems
;; (from Doom Emacs)
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you
      ;; scroll the cursor more than N lines past window edges (where
      ;; N is the settings of `scroll-conservatively'). This is
      ;; especially slow in larger files during large-scale scrolling
      ;; commands. If kept over 100, the window is never automatically
      ;; recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting
      ;; `window-vscroll' for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)

;; Title formatting
;; (from Doom Emacs)
(setq frame-title-format '("%b :: emacs")
      icon-title-format frame-title-format)

;; Get rid of GUI widgets again (this time working on Emacs 26)
;; (from Doom Emacs)
(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

;; Manually set these variables to nil to update what we just did above.
;; If - for some goddamn reason - someone wants to enable these
;; features, they won't need to call the mode toggle functions twice
;; to actually activate.
;; (from Doom Emacs)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Disable dialog boxes, if possible.
;; (from Doom Emacs)
(setq use-dialog-box nil)

;; Show what would be shown on tooltips in the echo-area.
;; (from Doom Emacs)
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Show the current key sequence in minibuffer more quickly.
;; (from Doom Emacs)
(setq echo-keystrokes 0.02)

;; Stop blinking cursors and parens
;; (from Doom Emacs)
(blink-cursor-mode -1)
(setq blink-matching-paren nil)

;; Prevent beeps
;; (from Doom Emacs)
(setq ring-bell-function #'ignore
      visible-bell nil)

;; Change the buffer name styles
;; (from Doom Emacs)
(setq uniquify-buffer-name-style 'forward)

;; Replace yes/no with y/n
;; (from Doom Emacs)
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; Some terminals offer two different cursors: a “visible” static
;; cursor and a “very visible” blinking one. By default, Emacs uses
;; the very visible cursor and switches to it when you start or resume
;; Emacs. If `visible-cursor' is nil when Emacs starts or resumes, it
;; uses the normal cursor.
;; (from Doom Emacs)
(setq visible-cursor nil)

;; Enable the use of minibuffers inside minibuffers.
;; (from Doom Emacs)
(setq enable-recursive-minibuffers t)

;; Expand the minibuffer to fit multi-line text displayed in the
;; echo-area, but also limit its height.
;; (from Doom Emacs)
(setq resize-mini-windows 'grow-only
      max-mini-window-height 0.15)

;; No double space after sentence.
;; (from Doom Emacs)
(setq sentence-end-double-space nil)

;; The "max" text width used here.
(setq fill-column 80)

;; Prefer spaces over tabs.
;; (from Doom Emacs)
(setq-default indent-tabs-mode nil
              tab-width 8)

;; Stretch the cursor for tab characters.
(setq x-stretch-cursor t)

;; Line highlighting
(global-hl-line-mode)

;; Disable automatic copy-to-clipboard behavior
(setq x-select-enable-clipboard nil)

;; Org config
(setq org-startup-indented t
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t
      org-fontify-done-headline t
      org-hide-leading-stars t
      org-pretty-entities t
      org-odd-levels-only t)

;; Remap ESC to cancelling commands
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
(define-key isearch-mode-map [escape] 'isearch-abort)
(define-key isearch-mode-map "\e" 'isearch-abort)

;; Recentf config
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

(dolist (x '("ç" "§" "Ã"))
  (define-key evil-motion-state-map x #'evil-ex))

(bind-map my-leader-map
  :evil-keys ("SPC")
  :bindings ("h" #'(lambda ()
                     (interactive)
                     (let ((default-directory "~"))
                       (call-interactively #'find-file)))
             "." #'find-file
             "SPC" #'counsel-recentf
             "e" #'eval-expression
             "E" #'eval-last-sexp
             "rr" #'ice-rifle-run
             "rb" #'ice-rifle-build
             "rt" #'ice-rifle-test
             "rc" #'ice-rifle-check
             "fc" #'(lambda ()
                      (interactive)
                      (find-file (f-join user-modules-directory "main.el")))
             "fd" #'dired
             "m" #'counsel-M-x
             "s" #'vr/replace
             "fb" #'format-all-buffer
             "b" #'counsel-switch-buffer
             "B" #'counsel-switch-buffer-other-window))

(dolist (x '(("p" #'evil-paste-after)
             ("P" #'evil-paste-before)
             ("y" #'evil-yank)
             ("Y" #'evil-yank-line)
             ("d" #'evil-delete)
             ("D" #'evil-delete-line)))
  (define-key my-leader-map (car x) `(lambda ()
                                       (interactive)
                                       (evil-use-register ?+)
                                       (call-interactively ,(car (cdr x))))))

(define-key evil-insert-state-map (kbd "C-y") #'evil-paste-after)

;; Handle ice-tty cursor changing on terminals
(inline-hook! 'after-make-frame-functions (_)
              (unless (display-graphic-p)
                (ice-tty-change-cursor)))
(inline-hook! 'evil-insert-state-entry-hook () (ice-tty-change-cursor 6))
(inline-hook! 'evil-operator-state-entry-hook () (ice-tty-change-cursor 3))
(add-hook 'evil-normal-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-motion-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-replace-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-visual-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-emacs-state-entry-hook #'ice-tty-change-cursor)

(ice-style-update)
(provide 'main)

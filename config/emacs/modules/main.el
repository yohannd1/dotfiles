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

;; Set options for ice-style.
(add-hook 'ice-style-before-update-hook
          #'(lambda ()
              (setq ice-style-current-theme 'base16
                    ice-style-font-family (get-xres "font" ice-style-font-family)
                    ice-style-font-height 100)))
(add-hook 'ice-style-after-update-hook #'centaur-tabs-headline-match)

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

;; Options for c-mode
(setq c-default-style "linux"
      c-basic-offset 4)

;; Options for sh-mode
(setq sh-basic-offset 2)

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
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling

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
              tab-width 4)

;; Stretch the cursor for tab characters.
(setq x-stretch-cursor t)

;; Line highlighting
(global-hl-line-mode)

;; Disable automatic copy-to-clipboard behavior
(setq x-select-enable-clipboard nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

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

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package git-commit-message
  :ensure nil
  :defer t
  :mode (("COMMIT_EDITMSG" . conf-unix-mode)))

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

(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (setq ac-auto-start 2)
  (setq ac-auto-show-menu t)
  (setq ac-use-menu-map t)
  (global-auto-complete-mode)
  (define-key ac-menu-map (kbd "<backtab>") 'ac-previous)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil))

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))
;; (define-key ivy-mode-map (kbd "ESC") #'ice-escape))

(use-package try
  :ensure t
  :defer t)

(use-package which-key ;; TODO: setup
  :ensure t
  :config
  (which-key-mode))

(use-package format-all
  :ensure t
  :defer t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package centaur-tabs ;; TODO: move & remake
  :ensure t
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-close-button nil
        centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "+"
        centaur-tabs-adjust-buffer-order t
        centaur-tabs-set-icons nil)
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
  (centaur-tabs-enable-buffer-reordering))

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

;; Remap ESC to cancelling commands
(define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; translation (currently testing to see if it works well)
(define-key key-translation-map (kbd "C-g") nil) ;; I don't want the pinky problem
(global-set-key [remap keyboard-quit] #'ice-escape)
(if (display-graphic-p) ;; on isearch
    (define-key isearch-mode-map [escape] 'isearch-abort)
  (define-key isearch-mode-map "\e" 'isearch-abort))

(defvar evil-leader-map (make-sparse-keymap)
  "A keymap with common functions.")

;; Switch to the leader map
(define-key evil-motion-state-map (kbd "SPC") evil-leader-map)

;; General commands
(define-key evil-motion-state-map (kbd "ç") #'evil-ex)
(define-key evil-motion-state-map (kbd "M-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") #'evil-window-right)
(define-key evil-motion-state-map (kbd "M-h") #'evil-window-left)
(define-key evil-leader-map (kbd ".") #'find-file)
(define-key evil-leader-map (kbd "e") #'eval-expression)
(define-key evil-motion-state-map (kbd "M-n") #'ido-switch-buffer-other-frame)
(define-key evil-motion-state-map (kbd "M-b") #'ido-switch-buffer)

;; Clipboard commands
(define-key evil-leader-map (kbd "p") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-paste-after)))
(define-key evil-leader-map (kbd "P") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-paste-before)))
(define-key evil-leader-map (kbd "y") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-yank)))
(define-key evil-leader-map (kbd "Y") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-yank-line)))
(define-key evil-leader-map (kbd "d") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-delete)))
(define-key evil-leader-map (kbd "D") #'(lambda ()
                                          (interactive)
                                          (evil-use-register ?+)
                                          (call-interactively #'evil-delete-line)))

;; Open url (with xdg-open)
(define-key evil-leader-map (kbd "o") #'browse-url-xdg-open)

;; Insert mode
(define-key evil-insert-state-map (kbd "C-y") #'evil-paste-after)

;; Rifle commands
(define-key evil-leader-map (kbd "rr") #'ice-rifle-run)

;; Config commands
(define-key evil-leader-map (kbd "cr") #'ice-config-reload)
(define-key evil-leader-map (kbd "ce") #'(lambda ()
                                           (interactive)
                                           (find-file (f-join user-modules-directory "main.el"))))

;; Etc.
(define-key evil-leader-map (kbd "s") #'vr/replace)
(define-key evil-leader-map (kbd "f") #'format-all-buffer)
(define-key global-map (kbd "C-j") #'centaur-tabs-forward)
(define-key global-map (kbd "C-k") #'centaur-tabs-backward)
(define-key evil-motion-state-map (kbd "C-j") #'centaur-tabs-forward)
(define-key evil-motion-state-map (kbd "C-k") #'centaur-tabs-backward)

;; Handle theme loading on clients.
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (ice-style-update)))))

;; Handle ice-tty cursor changing on terminals
(add-hook 'evil-normal-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-motion-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-replace-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-visual-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-insert-state-entry-hook (lambda () (ice-tty-change-cursor 6)))
(add-hook 'evil-operator-state-entry-hook (lambda () (ice-tty-change-cursor 3)))
(add-hook 'evil-emacs-state-entry-hook #'ice-tty-change-cursor)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil
                          tab-width 2)))

(unless (daemonp)
  (ice-style-update))

(provide 'main)

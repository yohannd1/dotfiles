;;; -*- lexical-binding: t; -*-

;; Set options for core-style.
(inline-hook! 'core-style-before-update-hook ()
              (setq core-style-current-theme 'base16
                    core-style-font-family (get-xres "font" core-style-font-family)
                    core-style-font-height 100))

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

;; Automatically create a file/buffer when called if it doesn't exist
(setq confirm-nonexistent-file-or-buffer nil)

;; Pixelwise resizing
(setq frame-resize-pixelwise t
      window-resize-pixelwise t)

;; No standard startup buffer
(setq inhibit-startup-message t)

;; Garbage collection stuff
(setq gc-cons-threshold (* 50 1000 1000))

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
(setq frame-title-format '("%b - emacs")
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
(global-hl-line-mode -1)

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

;; Treat _ as a word character
(modify-syntax-entry ?_ "w")

;; Recentf config
(setq recentf-max-menu-items 50)
(setq recentf-max-saved-items 50)

;; Transparency
;; (setq conf/alpha 90)
;; (set-frame-parameter (selected-frame) 'alpha `(,conf/alpha ,conf/alpha))
;; (add-to-list 'default-frame-alist `(alpha ,conf/alpha ,conf/alpha))

(use-package ace-window
  :ensure t
  :config
  (setq aw-keys '(?a ?s ?d ?f ?q ?w ?e ?r))
  (setq aw-dispatch-always t)
  (setq aw-minibuffer-flag t)
  (setq aw-background nil)
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?c aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

;; From Doom Emacs
;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it until
;;      later in the startup process and, for some reason, it runs much faster
;;      when it does.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (inline-hook! 'window-setup-hook ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t)))

;; get rid of scratch buffer
(defun my/default-buffer ()
  ;; http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
  (interactive)
  (let ((buffer (get-buffer-create "*default*")))
    (switch-to-buffer buffer)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buffer))

(setq initial-major-mode #'text-mode)

(when (daemonp)
  (setq initial-buffer-choice #'my/default-buffer)
  (kill-buffer "*scratch*"))

(provide 'conf-general)

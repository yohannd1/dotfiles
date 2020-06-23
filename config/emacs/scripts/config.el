(global-visual-line-mode 1)
(global-linum-mode 1)
(electric-pair-mode 1)

(unless (display-graphic-p)
  (xterm-mouse-mode))

(setq inhibit-startup-message t) ; Disable welcome message on startup

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

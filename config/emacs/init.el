;; -*- origami-fold-style: triple-braces -*-
;; vim: fdm=marker

(defvar my/init-amount -1
  "Indicates the amount of times the init file has been loaded.
Starts with -1 because, for convenience reasons, it is increased on the start of the file.")
(setq my/init-amount (+ my/init-amount 1))

;; Package Management {{{

;; Set up 'package and 'use-package {{{

(require 'package)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	;; ("melpa" . "http://melpa.org/packages")
	("gnu" . "http://elpa.gnu.org/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; }}}

;; Theme: 'atom-one-dark-theme {{{
(use-package atom-one-dark-theme
  :ensure t
  :defer t)
;; }}}
;; Evil Mode {{{
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "M-o") 'save-buffer-reload-term-dash)
    (define-key evil-motion-state-map (kbd "ç") 'evil-ex)
    (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
    (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)))
;; }}}
;; Evil-Commentary {{{
(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode 1))
;; }}}
;; Origami {{{
(use-package origami
  :ensure t
  :config (global-origami-mode 1)
  (defun nin-origami-toggle-node ()
    (interactive)
    (save-excursion ; leave point where it is
  (goto-char (point-at-eol)) ; then go to the end of line
  (origami-toggle-node (current-buffer) (point)))) ; and try to fold
  (define-key evil-normal-state-map (kbd "TAB") 'nin-origami-toggle-node)
  (define-key evil-normal-state-map (kbd "<backtab>") 'origami-close-all-nodes))
;; }}}
;; Rust Mode {{{
(use-package rust-mode
  :ensure t)
;; }}}
;; Haskell Mode {{{
(use-package haskell-mode
  :ensure t)
;; }}}
;; Markdown Mode {{{
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;; }}}
;; Clojure Mode {{{
(use-package clojure-mode
  :ensure t)
;; }}}
;; Julia Mode {{{
(use-package julia-mode
  :ensure t)
;; }}}
;; Typescript Mode {{{
(use-package typescript-mode
  :ensure t)
;; }}}
;; Rainbow Delimiters {{{
(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode 1))
;; }}}
;; Auto Complete {{{
(use-package auto-complete
  :ensure t
  :config
  (global-auto-complete-mode))
;; }}}
;; Helm {{{
(use-package helm
  :ensure t)
;; }}}
;; Try {{{
(use-package try
  :ensure t)
;; }}}
;; Which Key {{{
(use-package which-key
  :ensure t
  :config
  (which-key-mode))
;; }}}

;; }}}
;; Interactive Commands {{{

;; (rl): Reload {{{
(defun rl () "Reloads the init.el file stored in ~/.emacs.d"
       (interactive)
       (load-file "~/.emacs.d/init.el"))
;; }}}

;; }}}
;; Functions {{{

(defun my/term-change-cursor (&optional cursor-type)
  "Sends a escape code to the terminal indicating that the cursor should change."
  (if (eq cursor-type nil) (setq cursor-type 2))
  (when (not (display-graphic-p))
    (send-string-to-terminal (concat "\033[" (number-to-string cursor-type) " q"))))

;; }}}
;; Minor settings {{{

;; Modes to enable
(global-visual-line-mode 1)
(global-linum-mode 1)
(electric-pair-mode 1)

;; Modes to disable
(tool-bar-mode 0)
(menu-bar-mode 0)
(when (display-graphic-p)
  (scroll-bar-mode 0))

(setq inhibit-startup-message t) ; Disable welcome message on startup
;; (setq initial-scratch-message nil) ; Disable scratch buffer on startup

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

;; }}}
;; GUI or Terminal? {{{

(defun my/load-theme ()
  "Detects whether the current frame is on GUI or a terminal and then loads the corresponding theme."
  (if (display-graphic-p)
      (progn
	(load-theme my/theme-gui t)
	(set-frame-font "JetBrains Mono Medium 11" nil t))
    (progn
      (load-theme my/theme-term t))))

(defvar my/theme-gui 'atom-one-dark
  "The GUI theme to be used.")
(defvar my/theme-term 'term-dash
  "The terminal theme to be used.")

(when (eq my/init-amount 0)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/code/")
  (add-to-list 'load-path "~/.emacs.d/code/")
  (if (daemonp)
      (progn
	(add-hook 'after-make-frame-functions
		  (lambda (frame)
		    (with-selected-frame frame (my/load-theme)))))
    (progn
      (my/load-theme)))
  (if (display-graphic-p)
      nil
      (progn
	;; Evil hooks for changing cursor on mode change {{{
	(add-hook 'evil-normal-state-entry-hook #'my/term-change-cursor)
	(add-hook 'evil-motion-state-entry-hook #'my/term-change-cursor)
	(add-hook 'evil-replace-state-entry-hook #'my/term-change-cursor)
	(add-hook 'evil-visual-state-entry-hook #'my/term-change-cursor)
	(add-hook 'evil-insert-state-entry-hook (lambda () (my/term-change-cursor 6)))
	(add-hook 'evil-operator-state-entry-hook (lambda () (my/term-change-cursor 3)))
	(add-hook 'evil-emacs-state-entry-hook #'my/term-change-cursor)
	;; }}}
	)))

(if (display-graphic-p)
    ()
  (progn
    (xterm-mouse-mode)))

;; }}}

(evil-set-initial-state 'term-mode 'emacs)

(setq c-default-style "linux"
      c-basic-offset 4)

(defun save-buffer-reload-term-dash ()
  (interactive)
  (save-buffer)
  (load-theme 'term-dash t))

(custom-set-variables
 '(safe-local-variable-values (quote ((origami-fold-style . triple-braces)))))
(custom-set-faces)

(setq scroll-step 1
      scroll-margin 5)

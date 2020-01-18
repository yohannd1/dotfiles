;; -*- origami-fold-style: triple-braces -*-
;;; YohananDiamond's barely alive init.el file.

(defvar my/init-amount -1
  "Indicates the amount of times the init file has been loaded.
Starts with -1 because, for convenience reasons, it is increased on the start of the file.")
(setq my/init-amount (+ my/init-amount 1))

;; Package Management {{{

;; Set up 'package and 'use-package {{{

(require 'package)
(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages")
	("gnu" . "http://elpa.gnu.org/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; }}}
;; Actual packages {{{

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
;; Rainbow Delimiters {{{
;; TODO: Fix this (it doesn't seem to work at startup)
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
;; TODO: Actually use this
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

;; }}}
;; Interactive Commands {{{

;; rl -> Reload {{{
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
(scroll-bar-mode 0)

(setq inhibit-startup-message t) ; Disable welcome message on startup
;; (setq initial-scratch-message nil) ; Disable scratch buffer on startup

(setq vc-follow-symlinks t)
(setq linum-format "%3d ")

;; }}}
;; GUI or Terminal? {{{

(defun my/load-theme ()
  "Detects whether the current frame is on GUI or a terminal and then loads the corresponding theme."
  (if (display-graphic-p)
      (progn
	(load-theme my/theme-gui t)
	(set-frame-font "Cascadia Code 11" nil t))
    (progn
      (load-theme my/theme-term t))))

(defvar my/theme-gui 'atom-one-dark
  "The GUI theme to be used.")
(defvar my/theme-term 'term-dash
  "The terminal theme to be used.")

(when (eq my/init-amount 0)
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

;; }}}

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2992c7eeda3e58b1c19b3f0029fea302c969530cb5cf5904436d91d216993f39" default)))
 '(package-selected-packages
   (quote
    (clojure-mode which-key use-package try simpleclip rust-mode rainbow-delimiters origami markdown-mode helm haskell-mode evil-commentary auto-complete atom-one-dark-theme)))
 '(safe-local-variable-values (quote ((origami-fold-style . triple-braces)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

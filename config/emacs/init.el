;; -*- origami-fold-style: triple-braces -*-
;;; YohananDiamond's barely alive init.el file.
;;; Going to make this better later, I guess.
;;; I removed half of my config accidentally and it wasn't saved anywhere else.

;; Package Management {{{

;; Set up 'package and 'use-package {{{

(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages")
	("melpa-stable" . "http://stable.melpa.org/packages/")
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
  (send-string-to-terminal (concat "\033[" (number-to-string cursor-type) " q")))

;; }}}
;; Minor settings {{{

(global-visual-line-mode)
(global-linum-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t) ; Disable welcome message on startup
;; (setq initial-scratch-message nil) ; Disable scratch buffer on startup
(setq vc-follow-symlinks t)
(setq linum-format "%3d ")

;; }}}
;; GUI or Terminal? {{{

;; I'm tired.
;; Works well on all cases, except GUI emacsclient.
;; https://stackoverflow.com/questions/18904529/after-emacs-deamon-i-can-not-see-new-theme-in-emacsclient-frame-it-works-fr
;; Cases enum { term-standalone; term-client; gui-standalone; gui-client; daemon; }
(defun my/graphic-or-not (&optional frame)
  (if (display-graphic-p)
    ;; Theme & Font loading on GUI
    (if (daemonp)
    (if (eq frame nil)
    ;; If the command is called without args (probably simply from the call that is after this function), do nothing
    nil
  ;; On other cases, do the magic
  (with-selected-frame frame
	      (require 'atom-one-dark-theme)
	      (load-theme 'atom-one-dark t)
	      (set-frame-font "Cascadia Code 11" nil t)))
	(progn
  (load-theme 'atom-one-dark t)
  (set-frame-font "Cascadia Code 11" nil t)))
    (progn
  (my/term-change-cursor)
  (load-theme 'term-dash t)
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
(my/graphic-or-not)

  ;; Run the function above when a client connects
  (when (not (boundp 'my/added-client-hooks))
    (setq my/added-client-hooks nil))
  (when (not my/added-client-hooks)
    (add-to-list 'after-make-frame-functions #'my/graphic-or-not))

  ;; }}}

;; TODO: Try to remember the rest of the config that I exterminated -- I lost the damn cursor changer... :(
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("2992c7eeda3e58b1c19b3f0029fea302c969530cb5cf5904436d91d216993f39" default)))
 '(safe-local-variable-values (quote ((origami-fold-style . triple-braces)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

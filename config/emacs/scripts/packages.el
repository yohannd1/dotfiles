(require 'package)

(when at-startup
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package base16-theme
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "M-o") 'save-buffer-reload-term-dash)
    (define-key evil-motion-state-map (kbd "ç") 'evil-ex)
    (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
    (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
    (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
    (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left)))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

(use-package origami
  :ensure t
  :config
  (global-origami-mode 1)
  (defun nin-origami-toggle-node ()
    (interactive)
    (save-excursion ; leave point where it is
      (goto-char (point-at-eol)) ; then go to the end of line
      (origami-toggle-node (current-buffer) (point)))) ; and try to fold
  (define-key evil-normal-state-map (kbd "TAB") 'nin-origami-toggle-node)
  (define-key evil-normal-state-map (kbd "<backtab>") 'origami-close-all-nodes))

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
  (rainbow-delimiters-mode 1))

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

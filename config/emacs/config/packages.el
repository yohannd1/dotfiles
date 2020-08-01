(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode 1))

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

(use-package helm ;; TODO: move & remake
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
  :defer t)

(use-package centaur-tabs ;; TODO: move & remake
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
  (centaur-tabs-mode t))

;; (use-package linum-relative
;;   :ensure t
;;   :config
;;   (setq linum-relative-format "%3s ")
;;   (add-hook 'after-change-major-mode-hook #'linum-relative-mode)
;;   (add-hook 'minibuffer-setup-hook #'linum-relative-off))

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

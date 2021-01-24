;;; -*- lexical-binding: t; -*-

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  :config
  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t)
    (setq-default evil-undo-system 'undo-tree))
  (setq evil-ex-search-case 'sensitive)
  (global-undo-tree-mode)
  (evil-set-initial-state 'term-mode 'emacs)
  (define-key evil-insert-state-map (kbd "C-y") #'evil-paste-after)
  (define-key evil-insert-state-map (kbd "C-o") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)
  (evil-mode 1))

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

;; Handle core-tty cursor changing on terminals
(inline-hook! 'after-make-frame-functions (_)
              (unless (display-graphic-p)
                (core-tty-change-cursor)))
(inline-hook! 'evil-insert-state-entry-hook () (core-tty-change-cursor 6))
(inline-hook! 'evil-operator-state-entry-hook () (core-tty-change-cursor 3))
(add-hook 'evil-normal-state-entry-hook #'core-tty-change-cursor)
(add-hook 'evil-motion-state-entry-hook #'core-tty-change-cursor)
(add-hook 'evil-replace-state-entry-hook #'core-tty-change-cursor)
(add-hook 'evil-visual-state-entry-hook #'core-tty-change-cursor)
(add-hook 'evil-emacs-state-entry-hook #'core-tty-change-cursor)
(add-hook 'evil-emacs-state-entry-hook #'core-tty-change-cursor)

(provide 'conf-evil)

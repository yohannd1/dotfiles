;;; -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package nim-mode
  :ensure t
  :defer t
  :config
  (inline-hook! 'nim-mode-hook ()
                (setq tab-width 2)
                (auto-fill-mode 0)))

(use-package haskell-mode
  :ensure t
  :defer t)

(use-package zig-mode
  :ensure t
  :defer t
  :config
  (setq-default zig-format-on-save nil))

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook #'auto-fill-mode))

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

;; c-mode
(inline-hook! 'c-mode-hook ()
              (c-set-style "linux")
              (setq indent-tabs-mode t))

;; sh-mode
(setq sh-basic-offset 2)

;; c++-mode
(c-add-style
 "c++" '("stroustrup"
         ;; use spaces rather than tabs
         (indent-tabs-mode . nil)
         ;; indent by four spaces
         (c-basic-offset . 4)
         ;; custom indentation rules
         (c-offsets-alist . ((inline-open . 0)
                             (brace-list-open . 0)
                             (statement-case-open . +)))))

(inline-hook! 'c++-mode-hook ()
              (c-set-style "c++"))

;; asm-mode
(inline-hook! 'asm-mode ()
              (setq indent-tabs-mode t))

(inline-hook! 'after-change-major-mode-hook ()
             (setq evil-shift-width tab-width))

(provide 'conf-fmodes)

;;; -*- lexical-binding: t; -*-

;; gmch - improve garbage collection for better startup times
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

;; f - a library for improved file/directory management
(use-package f
  :ensure t)

(provide 'core-packages)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
;(package-refresh-contents) ;; TODO: see if this is making everything bad to use.

(unless (display-graphic-p)
  (send-string-to-terminal "\033[2 q"))

;; Download Evil
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'evil)
  (package-install 'evil))
; (unless (package-installed-p 'helm)
;   (package-install 'helm))

;; Enable Evil
(require 'evil)
(evil-mode 1)

;; Mappings
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "ç") 'evil-ex)
  (define-key evil-motion-state-map (kbd "M-j") 'evil-window-down)
  (define-key evil-motion-state-map (kbd "M-k") 'evil-window-up)
  (define-key evil-motion-state-map (kbd "M-l") 'evil-window-right)
  (define-key evil-motion-state-map (kbd "M-h") 'evil-window-left))

;; Commands
(defun rl () "Reloads the init.el file stored in ~/.emacs.d"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Other settings
(global-visual-line-mode)
(global-linum-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package helm evil-c evil async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

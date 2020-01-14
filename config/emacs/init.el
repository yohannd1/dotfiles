(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
;(package-refresh-contents) ;; TODO: see if this is making everything bad to use.

;; Download Evil
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

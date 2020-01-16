(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(package-initialize)
(custom-set-variables
 '(package-selected-packages (quote (rust-mode use-package helm evil-c evil async))))
(custom-set-faces)

(defun emacsclient-code (_)
  "Code that should be ran when a client connects to the daemon or something like."
  (unless (display-graphic-p)
    (send-string-to-terminal "\033[2 q")))

(add-to-list 'after-make-frame-functions 'emacsclient-code)

;; Download Evil
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(unless (package-installed-p 'evil)
  (package-install 'evil))

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
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-visual-line-mode)
(global-linum-mode)

;; My personal emacs config!
;; Big inspirations for this config:
;; - Doom Emacs (https://github.com/hlissner/doom-emacs) :: stole some things from here.

;; Set startup variable to false if it has been defined previously
(when (boundp 'at-startup)
  (setq at-startup nil))

(defvar at-startup t
  "Whether the config file is being ran for the first time in the session.")

(defvar emacs-folder
  (file-name-directory (or load-file-name buffer-file-name))
  "The folder for all emacs data (usually ~/.emacs.d/).")

(defvar config-folder (concat emacs-folder "/config")
  "The main config folder.")

;; Add places to the load path
(when at-startup
  (add-to-list 'custom-theme-load-path (concat emacs-folder "/themes"))
  (add-to-list 'load-path (concat emacs-folder "/elisp")))

;; Load core and main configuration
(load "core.el")
(load "main.el")

;; Had to place this here - before the theme loading - for "safe theme" signature issues
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b9e70dbee8f49f04267996eed77952437f907cb3779ec5a6d038ad7088a167be" "e0a9d1720344dc44811485a995fa11f29381969867cef31e5c6a64903407157d" "217d1864eca46ea22433c883b2d85708f3e2d0b19eb386ad23811f22747ab487" "692b0cc3eca9b5ee4120cfaaab187e1d0c63dddeb60faa42c3484ca12f50a9aa" "2eb665bb8b4808d9c3a9543f8c11f8515c6417a6b1e045e1228bdbb0c0fa9834" "a00ac5fd7edbe19f1a7a033a42607ef9c6276b66e40343dc46f12f20ba60c0b7" "9220f366ae57a80c59bf31acb7eec920ce81f73cb0fcbdf981f0015ecfe107d1" "562e4c11ed52df7d3962c908cea8ed3b395ad977dbd8b19f693a59c51d22d009" "3d038886650c9c7471cea531abecd2e0919a332190d93f3aedb27c21153ce3f4" "f61782a665e880c097024e6c4352b2b533197eef17bc9539d8fb76c5bdc1ebcb" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "04a400a5130f31a54fa871c0409b7a457bb69a42def002a5117456e224210d09" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" default))
 '(package-selected-packages
   '(format-all centaur-tabs linum-relative which-key use-package typescript-mode try rust-mode rainbow-delimiters origami markdown-mode lua-mode julia-mode helm haskell-mode evil-commentary csharp-mode clojure-mode base16-theme auto-complete)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Some messy hook code
(when at-startup
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (with-selected-frame frame
		    (core/load-theme)
		    (unless (display-graphic-p)
		      (define-key evil-motion-state-map (kbd "ç") #'evil-ex)))))
    (core/load-theme))

  (unless (display-graphic-p)
    ;; Cursor changing code for terminal
    (add-hook 'evil-normal-state-entry-hook #'terminal-change-cursor)
    (add-hook 'evil-motion-state-entry-hook #'terminal-change-cursor)
    (add-hook 'evil-replace-state-entry-hook #'terminal-change-cursor)
    (add-hook 'evil-visual-state-entry-hook #'terminal-change-cursor)
    (add-hook 'evil-insert-state-entry-hook (lambda () (terminal-change-cursor 6)))
    (add-hook 'evil-operator-state-entry-hook (lambda () (terminal-change-cursor 3)))
    (add-hook 'evil-emacs-state-entry-hook #'terminal-change-cursor)))

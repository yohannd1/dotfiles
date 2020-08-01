;; Ice Library
;; A simple interface for easier configuration.

;; Platform-related constants
(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-WIN   (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-LINUX (eq system-type 'gnu-linux))
(defconst TERMINAL (if IS-WIN
		       "cmd"
		     (or (getenv "TERMINAL") "xterm")))

;; Some other constants
(defconst user-cache-directory (concat user-emacs-directory "cache/")
  "The place where general cache should be stored.
To be honest, almost no program supports this.")

;; Set up `package' and `use-package'
(when at-startup
  ;; `package'
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  ;; `use-package'
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Require some "essential" packages
(use-package f ;; file/directory functions
  :ensure t)

;; Load relevant modules
(load-here "ice-style.el")
(load-here "ice-utils.el")
(load-here "ice-tty.el")
(load-here "ice-rifle.el")

(defun ice-finish ()
  "Make some final touches on emacs after configuring."
  (unless (daemonp)
  (ice--style-update))) ;; apply the style changes


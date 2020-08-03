;; A small package that defines some useful constants and loads essential libraries.
;; It probably doesn't ever need to be required other than in main.el.

;; Some constants that might help when writing portable code.
(defconst EMACS27+ (> emacs-major-version 26))
(defconst IS-WIN   (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst TERMINAL (if IS-WIN "cmd"
                     (or (getenv "TERMINAL") "xterm")))

;; Load `package.el'
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Load `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; File/directory management functions
(use-package f
  :ensure t)

(provide 'ice-essentials)

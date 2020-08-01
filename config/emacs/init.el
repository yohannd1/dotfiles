;; Ice Emacs
;; My personal emacs "distro"!
;;
;; Big inspirations for this config are:
;; - Doom Emacs (https://github.com/hlissner/doom-emacs) - I literally stole code from here.

;; Set startup variable to false if it has been defined previously
(when (boundp 'at-startup)
  (setq at-startup nil))

(defvar at-startup t
  "Whether the config file is being ran for the first time in the session.")

(let (file-name-handler-alist) ;; ensure emacs is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

(defconst user-config-directory (concat user-emacs-directory "config/")
  "The directory where most of the configuration is stored.")

(when at-startup ;; add places to the load path
  (add-to-list 'load-path user-config-directory)
  (add-to-list 'custom-theme-load-path (concat user-config-directory "themes/")))

(defun load-here (file)
  "Loads a file based on the current dir."
  (load (concat (file-name-directory load-file-name) file)))

;; Set the location of the customizations file.
(setq custom-file (concat user-config-directory "custom.el"))

;; Load relevant files and functions
(load-here "config/ice/ice.el")  ;; load library
(load-here custom-file)          ;; custom file
(load-here "config/main.el")     ;; main config
(ice-finish)                     ;; end touches

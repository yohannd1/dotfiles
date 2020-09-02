;; -*- lexical-binding: t; -*-
;; My personal emacs configuration.
;;
;; Some big inspirations for this config:
;;   Doom Emacs (https://github.com/hlissner/doom-emacs) - I literally stole code from here.

;; Ensure emacs is running out of this file's directory.
;; Useful if loading this configuration with the --load option.
(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Define the directory where all modules are stored.
;; A module is basically a file with elisp code. I'm placing most of
;; them here because it is more practical having all of them inside a
;; single folder.
;; This directory is used by functions like `require' or `load-theme'.
(defconst user-modules-directory (concat user-emacs-directory "modules/")
  "The directory for all the modules.")

(defconst user-cache-directory (concat user-emacs-directory "cache/")
  "The place where general cache should be stored.")

;; Add the modules directory to the load path
(add-to-list 'load-path user-modules-directory)              ;; require
(add-to-list 'custom-theme-load-path user-modules-directory) ;; load-theme

;; Set the location of the customizations file.
;; I prefer to not to have this configuration synced between machines,
;; so I'm not tracking it in my dotfiles.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Load main.el in the modules directory.
;; It contains most of my configuration and the call for other libraries.
(require 'main)

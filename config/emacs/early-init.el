;;; -*- lexical-binding: t; -*-
;;;
;;; Early Init File
;;; This file runs on newer versions of emacs, before the frame is rendered.
;;; Let's use this in our favor.

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore manual loading of X resources.
(advice-add #'x-apply-session-resources :override #'ignore)

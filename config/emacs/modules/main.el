;;; -*- lexical-binding: t; -*-
;;; "Actual" configuration file

(require 'core-init)

(core/load!
 ;; core modules
 core-packages
 core-style
 core-misc
 core-rifle
 ;; configuration modules
 conf-packages
 conf-evil
 conf-fmodes
 conf-kb
 conf-general)

(core-style-update)

(message "Core & Config Loaded! Took (%.3fs)"
         (core/time-since-start))

(provide 'main)

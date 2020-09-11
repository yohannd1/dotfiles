;;; -*- lexical-binding: t; -*-
;;; "Actual" configuration file

(require 'core-init)

(core/load!
 core-packages
 core-style
 core-misc
 core-rifle
 conf-packages
 conf-evil
 conf-fmodes
 conf-general)

(core-style-update)

(provide 'main)

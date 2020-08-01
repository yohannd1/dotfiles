;; Ice Emacs main configuration file
;; 
;; This file (and the ones called by it) focus more on loading
;; packages and setting options, in contrast to ice.el, where most
;; things are functions, constants and default options.

;; Style options
(setq ice-style-theme 'base16
      ice-style-font "JetBrains Mono Medium 10")

;; Set some options, like disabling cursor blink or disabling symlink
;; follows.
(load-here "options.el")

;; Require packages (with `use-package') and set specific
;; configurations related to them.
(load-here "packages.el")

;; Define most of the keybindings here.
;; TODO: make a map macro like on Doom
(load-here "keybindings.el")

;; Setup hooks, only at startup.
(when at-startup
  (load-here "hooks.el"))

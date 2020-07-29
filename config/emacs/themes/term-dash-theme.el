(deftheme term-dash
 "Yohanan's dashy terminal theme.")

(setq
  base00 "black"
  base01 "brightgreen"
  base02 "brightyellow"
  base03 "brightblack"
  base04 "brightblue"
  base05 "white"
  base06 "brightmagenta"
  base07 "brightwhite"
  base08 "red"
  base09 "brightred"
  base0A "yellow"
  base0B "green"
  base0C "cyan"
  base0D "blue"
  base0E "magenta"
  base0F "brightcyan")

(custom-theme-set-faces 'term-dash
 '(default ((t (:background "color-0" :foreground "5"))))
 '(minibuffer-prompt ((t (:foreground "2"))))
 '(trailing-whitespace ((t (:background "3"))))
 '(link ((t (:foreground "13" :bold t))))
 '(link-visited ((t (:inherit link :foreground "12" :bold t))))
 '(region ((t (:background "color-2" :foreground "5"))))

 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-warning-face ((t (:background "red" :foreground "black"))))
 '(font-lock-comment-face ((t (:foreground "color-245"))))
 '(font-lock-doc-face ((t (:foreground "color-248"))))
 '(font-lock-builtin-face ((t (:foreground "magenta"))))
 '(font-lock-constant-face ((t (:foreground "yellow"))))
 '(font-lock-keyword-face ((t (:foreground "red"))))
 '(font-lock-function-name-face ((t (:foreground "blue"))))
 '(font-lock-string-face ((t (:foreground "green"))))
 '(font-lock-type-face ((t (:foreground "yellow"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))
 '(font-lock-preprocessor-face ((t (:foreground "yellow"))))
 '(mode-line ((t (:background "black"))))
 '(mode-line-inactive ((t (:inherit 'mode-line :background "color-239"))))
 '(mode-line-emphasis ((t (:background "magenta" :foreground "black" :bold t))))
 '(mode-line-buffer-id ((t (:bold nil))))
 '(mode-line-highlight ((t (:bold t))))
 )

;;    '(escape-glyph ((t (:foreground "yellow"))))
;;    '(homoglyph ((t (:foreground "yellow"))))
;;    '(highlight ((t (:background "magenta" :foreground "black"))))
;;    '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "grey50")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "grey70")) (((class color) (min-colors 8) (background light)) (:foreground "green")) (((class color) (min-colors 8) (background dark)) (:foreground "yellow"))))
;;    '(secondary-selection ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 16) (background light)) (:background "yellow")) (((class color) (min-colors 16) (background dark)) (:background "SkyBlue4")) (((class color) (min-colors 8)) (:foreground "black" :background "cyan")) (t (:inverse-video t))))
;;    '(button ((t (:background "blue" :foreground "black"))))
;;    '(fringe ((((class color) (background light)) (:background "grey95")) (((class color) (background dark)) (:background "grey10")) (t (:background "gray"))))
;;    '(header-line ((t (:underline (:color foreground-color :style line) :inverse-video nil :inherit (mode-line)))))
;;    '(tooltip ((t (:foreground "black" :background "lightyellow" :inherit (variable-pitch)))))
;;    '(isearch ((t (:background "yellow" :foreground "black"))))
;;    '(isearch-fail ((t (:background "red" :foreground "black"))))
;;    '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
;;    '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
;;    '(next-error ((t (:inherit (region)))))
;;    '(query-replace ((t (:background "color-240" :foreground "white"))))))

(provide-theme 'term-dash)

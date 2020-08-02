;; base16 theme (custom version)
;; Based off: https://github.com/belak/base16-emacs/blob/master/base16-theme.el
;; 
;; GUI version only works on X (it uses Xresources)
;; Terminal version only has been seen working with TERM=xterm-16color

(deftheme base16
  "A custom base16 theme that works on my machines.")

(setq base16-fallback-base00 "#f2e5bc"
      base16-fallback-base01 "#ebdbb2"
      base16-fallback-base02 "#d5c4a1"
      base16-fallback-base03 "#bdae93"
      base16-fallback-base04 "#665c54"
      base16-fallback-base05 "#504945"
      base16-fallback-base06 "#3c3836"
      base16-fallback-base07 "#282828"
      base16-fallback-base08 "#9d0006"
      base16-fallback-base09 "#af3a03"
      base16-fallback-base0A "#b57614"
      base16-fallback-base0B "#79740e"
      base16-fallback-base0C "#427b58"
      base16-fallback-base0D "#076678"
      base16-fallback-base0E "#8f3f71"
      base16-fallback-base0F "#d65d0e")

(if (display-graphic-p)
    (setq base00 (ice-get-xres "base00" base16-fallback-base00)
          base01 (ice-get-xres "base01" base16-fallback-base01)
          base02 (ice-get-xres "base02" base16-fallback-base02)
          base03 (ice-get-xres "base03" base16-fallback-base03)
          base04 (ice-get-xres "base04" base16-fallback-base04)
          base05 (ice-get-xres "base05" base16-fallback-base05)
          base06 (ice-get-xres "base06" base16-fallback-base06)
          base07 (ice-get-xres "base07" base16-fallback-base07)
          base08 (ice-get-xres "base08" base16-fallback-base08)
          base09 (ice-get-xres "base09" base16-fallback-base09)
          base0A (ice-get-xres "base0A" base16-fallback-base0A)
          base0B (ice-get-xres "base0B" base16-fallback-base0B)
          base0C (ice-get-xres "base0C" base16-fallback-base0C)
          base0D (ice-get-xres "base0D" base16-fallback-base0D)
          base0E (ice-get-xres "base0E" base16-fallback-base0E)
          base0F (ice-get-xres "base0F" base16-fallback-base0F))
  (setq base00 "black"
        base01 "red"
        base02 "green"
        base03 "yellow"
        base04 "blue"
        base05 "purple"
        base06 "cyan"
        base07 "white"
        base08 "brightblack"
        base09 "brightred"
        base0A "brightgreen"
        base0B "brightyellow"
        base0C "brightblue"
        base0D "brightpurple"
  	    base0E "brightcyan"
  	    base0F "brightwhite"))

(defun base16--apply-specs (spec-list)
  (dolist (spec spec-list)
    (custom-theme-set-faces 'base16 `(,(car spec) ((t ,(cdr spec)))))))

(base16--apply-specs
 `((border                                       :background ,base03)
   (cursor                                       :background ,base0D)
   (default                                      :foreground ,base05 :background ,base00)
   (fringe                                       :background ,base01)
   (gui-element                                  :background ,base01)
   (header-line                                  :foreground ,base0E :background nil :inherit mode-line)
   (highlight                                    :background ,base01)
   (link                                         :foreground ,base0D :underline t)
   (link-visited                                 :foreground ,base0E :underline t)
   (minibuffer-prompt                            :foreground ,base0D)
   (region                                       :background ,base02 :distant-foreground ,base05)
   (secondary-selection                          :background ,base03 :distant-foreground ,base05)
   (trailing-whitespace                          :foreground ,base0A :background ,base0C)
   (vertical-border                              :foreground ,base02)
   (widget-button                                :underline t)
   (widget-field                                 :background ,base03 :box (:line-width 1 :color ,base06))

   (error                                        :foreground ,base08 :weight bold)
   (warning                                      :foreground ,base09 :weight bold)
   (success                                      :foreground ,base0B :weight bold)
   (shadow                                       :foreground ,base03)

   (compilation-column-number                    :foreground ,base0A)
   (compilation-line-number                      :foreground ,base0A)
   (compilation-message-face                     :foreground ,base0D)
   (compilation-mode-line-exit                   :foreground ,base0B)
   (compilation-mode-line-fail                   :foreground ,base08)
   (compilation-mode-line-run                    :foreground ,base0D)
   (show-paren-match                             :background ,base03)

   (custom-variable-tag                          :foreground ,base0D)
   (custom-group-tag                             :foreground ,base0D)
   (custom-state                                 :foreground ,base0B)

   ;; Font Lock
   (font-lock-builtin-face                       :foreground ,base0C)
   (font-lock-comment-delimiter-face             :foreground ,base02)
   (font-lock-comment-face                       :foreground ,base03)
   (font-lock-constant-face                      :foreground ,base09)
   (font-lock-doc-face                           :foreground ,base04)
   (font-lock-doc-string-face                    :foreground ,base03)
   (font-lock-function-name-face                 :foreground ,base0D)
   (font-lock-keyword-face                       :foreground ,base0E)
   (font-lock-negation-char-face                 :foreground ,base0B)
   (font-lock-preprocessor-face                  :foreground ,base0D)
   (font-lock-regexp-grouping-backslash          :foreground ,base0A)
   (font-lock-regexp-grouping-construct          :foreground ,base0E)
   (font-lock-string-face                        :foreground ,base0B)
   (font-lock-type-face                          :foreground ,base0A)
   (font-lock-variable-name-face                 :foreground ,base08)
   (font-lock-warning-face                       :foreground ,base08)

   ;; ISearch
   (match                                        :foreground ,base0D :background ,base01 :inverse-video t)
   (isearch                                      :foreground ,base0A :background ,base01 :inverse-video t)
   (lazy-highlight                               :foreground ,base0C :background ,base01 :inverse-video t)
   (isearch-lazy-highlight-face                  :inherit lazy-highlight) ;; was replaced with 'lazy-highlight in emacs 22
   (isearch-fail                                 :background ,base01 :inverse-video t :inherit font-lock-warning-face)

   ;; Line Numbers
   (line-number                                  :foreground ,base03 :background ,base01)
   (line-number-current-line                     :background ,base02)

   ;; modeline
   (mode-line                                    :foreground ,base05 :background ,base02) ; :box base16-settings-mode-line-box)
   (mode-line-buffer-id                          :foreground ,base0B :background nil)
   (mode-line-emphasis                           :foreground ,base06 :slant italic)
   (mode-line-highlight                          :foreground ,base0E :box nil :weight bold)
   (mode-line-inactive                           :foreground ,base03 :background ,base01 :box nil)

   ;; Third party:

   ;; linum-relative
   (linum-relative-current-face                  :foreground ,base05 :background ,base02)

   ;; anzu-mode
   (anzu-mode-line                               :foreground ,base0E)

   ;; auctex
   (font-latex-bold-face                         :foreground ,base0B)
   (font-latex-doctex-documentation-face         :background ,base03)
   (font-latex-italic-face                       :foreground ,base0B)
   (font-latex-math-face                         :foreground ,base09)
   (font-latex-sectioning-0-face                 :foreground ,base0A)
   (font-latex-sectioning-1-face                 :foreground ,base0A)
   (font-latex-sectioning-2-face                 :foreground ,base0A)
   (font-latex-sectioning-3-face                 :foreground ,base0A)
   (font-latex-sectioning-4-face                 :foreground ,base0A)
   (font-latex-sectioning-5-face                 :foreground ,base0A)
   (font-latex-sedate-face                       :foreground ,base0C)
   (font-latex-string-face                       :foreground ,base0A)
   (font-latex-verbatim-face                     :foreground ,base09)
   (font-latex-warning-face                      :foreground ,base08)

   (TeX-error-description-error                  :inherit error)
   (TeX-error-description-tex-said               :inherit font-lock-function-name-face)
   (TeX-error-description-warning                :inherit warning)

   ;; centaur-tabs
   (centaur-tabs-default                         :background ,base00 :foreground ,base05)
   (centaur-tabs-selected                        :background ,base01 :foreground ,base06)
   (centaur-tabs-unselected                      :background ,base00 :foreground ,base05)
   (centaur-tabs-selected-modified               :background ,base01 :foreground ,base0D)
   (centaur-tabs-unselected-modified             :background ,base00 :foreground ,base0D)
   (centaur-tabs-active-bar-face                 :background ,base00)
   (centaur-tabs-modified-marker-selected        :inherit 'centaur-tabs-selected :foreground ,base0D)
   (centaur-tabs-modified-marker-unselected      :inherit 'centaur-tabs-unselected :foreground ,base0D)
   ;; centaur-tabs-close-unselected
   ;; centaur-tabs-close-selected
   ;; centaur-tabs-close-mouse-face

   ;; circe-mode
   (circe-fool-face                              :foreground ,base02)
   (circe-my-message-face                        :foreground ,base0B)
   (circe-highlight-nick-face                    :foreground ,base0A)
   (circe-originator-face                        :foreground ,base0E)
   (circe-prompt-face                            :foreground ,base0D)
   (circe-server-face                            :foreground ,base03)

   ;; avy
   (avy-lead-face-0                              :foreground ,base00 :background ,base0C)
   (avy-lead-face-1                              :foreground ,base00 :background ,base05)
   (avy-lead-face-2                              :foreground ,base00 :background ,base0E)
   (avy-lead-face                                :foreground ,base00 :background ,base09)
   (avy-background-face                          :foreground ,base03)
   (avy-goto-char-timer-face                     :inherit highlight)

   ;; clojure-mode
   (clojure-keyword-face                         :foreground ,base0E)

   ;; company-mode
   (company-tooltip                              :inherit tooltip)
   (company-scrollbar-bg                         :background ,base07)
   (company-scrollbar-fg                         :background ,base04)
   (company-tooltip-annotation                   :foreground ,base08)
   (company-tooltip-common                       :inherit font-lock-constant-face)
   (company-tooltip-selection                    :background ,base02 :inherit font-lock-function-name-face)
   (company-tooltip-search                       :inherit match)
   (company-tooltip-search-selection             :inherit match)
   (company-preview-common                       :inherit secondary-selection)
   (company-preview                              :foreground ,base04)
   (company-preview-search                       :inherit match)
   (company-echo-common                          :inherit secondary-selection)

   ;; cperl-mode
   (cperl-array-face                             :weight bold :inherit font-lock-variable-name-face)
   (cperl-hash-face                              :weight bold :slant italic :inherit font-lock-variable-name-face)
   (cperl-nonoverridable-face                    :inherit font-lock-builtin-face)

   ;; cscope-minor-mode
   (cscope-file-face                             :foreground ,base0B)
   (cscope-function-face                         :foreground ,base0D)
   (cscope-line-number-face                      :foreground ,base0A)
   (cscope-mouse-face                            :foreground ,base04 :background ,base01)
   (cscope-separator-face                        :foreground ,base08 :overline t :underline t :weight bold)

   ;; csv-mode
   (csv-separator-face                           :foreground ,base09)

   ;; diff-hl-mode
   (diff-hl-change                               :foreground ,base0E)
   (diff-hl-delete                               :foreground ,base08)
   (diff-hl-insert                               :foreground ,base0B)

   ;; diff-mode
   (diff-added                                   :foreground ,base0B)
   (diff-changed                                 :foreground ,base0E)
   (diff-removed                                 :foreground ,base08)
   (diff-header                                  :background ,base01)
   (diff-file-header                             :background ,base02)
   (diff-hunk-header                             :foreground ,base0E :background ,base01)

   ;; dired+
   (diredp-compressed-file-suffix                :foreground ,base0D)
   (diredp-dir-heading                           :foreground nil :background nil :inherit heading)
   (diredp-dir-priv                              :foreground ,base0C :background nil)
   (diredp-exec-priv                             :foreground ,base0D :background nil)
   (diredp-executable-tag                        :foreground ,base08 :background nil)
   (diredp-file-name                             :foreground ,base0A)
   (diredp-file-suffix                           :foreground ,base0B)
   (diredp-flag-mark-line                        :background nil :inherit highlight)
   (diredp-ignored-file-name                     :foreground ,base04)
   (diredp-link-priv                             :foreground ,base0E :background nil)
   (diredp-mode-line-flagged                     :foreground ,base08)
   (diredp-mode-line-marked                      :foreground ,base0B)
   (diredp-no-priv                               :background nil)
   (diredp-number                                :foreground ,base0A)
   (diredp-other-priv                            :foreground ,base0E :background nil)
   (diredp-rare-priv                             :foreground ,base08 :background nil)
   (diredp-read-priv                             :foreground ,base0B :background nil)
   (diredp-symlink                               :foreground ,base0E)
   (diredp-write-priv                            :foreground ,base0A :background nil)

   ;; doom-modeline
   (doom-modeline-eldoc-bar                      :background ,base0B)
   (doom-modeline-inactive-bar                   :background nil) ; transparent
   (doom-modeline-bar                            :background ,base0D)

   ;; ediff-mode
   (ediff-even-diff-A                            :foreground nil :background nil :inverse-video t)
   (ediff-even-diff-B                            :foreground nil :background nil :inverse-video t)
   (ediff-odd-diff-A                             :foreground ,base04 :background nil :inverse-video t)
   (ediff-odd-diff-B                             :foreground ,base04 :background nil :inverse-video t)

   ;; eldoc-mode
   (eldoc-highlight-function-argument            :foreground ,base0B :weight bold)

   ;; erc
   (erc-direct-msg-face                          :foreground ,base09)
   (erc-error-face                               :foreground ,base08)
   (erc-header-face                              :foreground ,base06 :background ,base04)
   (erc-input-face                               :foreground ,base0B)
   (erc-keyword-face                             :foreground ,base0A)
   (erc-current-nick-face                        :foreground ,base0B)
   (erc-my-nick-face                             :foreground ,base0B)
   (erc-nick-default-face                        :foreground ,base0E :weight normal)
   (erc-nick-msg-face                            :foreground ,base0A :weight normal)
   (erc-notice-face                              :foreground ,base04)
   (erc-pal-face                                 :foreground ,base09)
   (erc-prompt-face                              :foreground ,base0D)
   (erc-timestamp-face                           :foreground ,base0C)

   ;; eshell
   (eshell-ls-archive                            :foreground ,base08)
   (eshell-ls-backup                             :foreground ,base0F)
   (eshell-ls-clutter                            :foreground ,base09)
   (eshell-ls-directory                          :foreground ,base0D)
   (eshell-ls-executable                         :foreground ,base0B)
   (eshell-ls-missing                            :foreground ,base08)
   (eshell-ls-product                            :foreground ,base0F)
   (eshell-ls-readonly                           :foreground ,base06)
   (eshell-ls-special                            :foreground ,base0E)
   (eshell-ls-symlink                            :foreground ,base0C)
   (eshell-ls-unreadable                         :foreground ,base04)
   (eshell-prompt                                :foreground ,base05)

   ;; evil-mode
   (evil-search-highlight-persist-highlight-face :background ,base01 :inverse-video t :inherit font-lock-warning-face)

   ;; fic-mode
   (fic-author-face                              :foreground ,base09 :underline t)
   (fic-face                                     :foreground ,base08 :weight bold)

   ;; flycheck-mode
   (flycheck-error                               :underline (:style wave :color ,base08))
   (flycheck-info                                :underline (:style wave :color ,base0B))
   (flycheck-warning                             :underline (:style wave :color ,base09))

   ;; flymake-mode
   (flymake-warnline                             :background ,base01 :underline ,base09)
   (flymake-errline                              :background ,base01 :underline ,base08)
   (flymake-warning                              :background ,base01 :underline ,base09)
   (flymake-error                                :background ,base01 :underline ,base08)

   ;; flyspell-mode
   (flyspell-duplicate                           :underline (:style wave :color ,base09))
   (flyspell-incorrect                           :underline (:style wave :color ,base08))

   ;; git-gutter-mode
   (git-gutter:added                             :foreground ,base0B)
   (git-gutter:deleted                           :foreground ,base08)
   (git-gutter:modified                          :foreground ,base0E)
   (git-gutter:separator                         :foreground ,base0C)
   (git-gutter:unchanged                         :foreground ,base0A :inverse-video t)

   ;; git-gutter+-mode
   (git-gutter+-added                            :foreground ,base0B)
   (git-gutter+-deleted                          :foreground ,base08)
   (git-gutter+-modified                         :foreground ,base0E)
   (git-gutter+-unchanged                        :foreground ,base0A :inverse-video t)

   ;; git-gutter-fringe
   (git-gutter-fr:added                          :foreground ,base0B)
   (git-gutter-fr:deleted                        :foreground ,base08)
   (git-gutter-fr:modified                       :foreground ,base0E)

   ;; gnus
   (gnus-cite-1                                  :foreground nil :inherit outline-1)
   (gnus-cite-2                                  :foreground nil :inherit outline-2)
   (gnus-cite-3                                  :foreground nil :inherit outline-3)
   (gnus-cite-4                                  :foreground nil :inherit outline-4)
   (gnus-cite-5                                  :foreground nil :inherit outline-5)
   (gnus-cite-6                                  :foreground nil :inherit outline-6)
   (gnus-cite-7                                  :foreground nil :inherit outline-7)
   (gnus-cite-8                                  :foreground nil :inherit outline-8)
   ;; there are several more -cite- faces...
   (gnus-header-content                          :inherit message-header-other)
   (gnus-header-subject                          :inherit message-header-subject)
   (gnus-header-from                             :foreground ,base09 :weight bold :inherit message-header-other-face)
   (gnus-header-name                             :inherit message-header-name)
   (gnus-button                                  :foreground nil :inherit link)
   (gnus-signature                               :inherit font-lock-comment-face)

   (gnus-summary-normal-unread                   :foreground ,base0D :weight normal)
   (gnus-summary-normal-read                     :foreground ,base06 :weight normal)
   (gnus-summary-normal-ancient                  :foreground ,base0C :weight normal)
   (gnus-summary-normal-ticked                   :foreground ,base09 :weight normal)
   (gnus-summary-low-unread                      :foreground ,base04 :weight normal)
   (gnus-summary-low-read                        :foreground ,base04 :weight normal)
   (gnus-summary-low-ancient                     :foreground ,base04 :weight normal)
   (gnus-summary-high-unread                     :foreground ,base0A :weight normal)
   (gnus-summary-high-read                       :foreground ,base0B :weight normal)
   (gnus-summary-high-ancient                    :foreground ,base0B :weight normal)
   (gnus-summary-high-ticked                     :foreground ,base09 :weight normal)
   (gnus-summary-cancelled                       :foreground ,base08 :background nil :weight normal)

   (gnus-group-mail-low                          :foreground ,base04)
   (gnus-group-mail-low-empty                    :foreground ,base04)
   (gnus-group-mail-1                            :foreground nil :weight normal :inherit outline-1)
   (gnus-group-mail-2                            :foreground nil :weight normal :inherit outline-2)
   (gnus-group-mail-3                            :foreground nil :weight normal :inherit outline-3)
   (gnus-group-mail-4                            :foreground nil :weight normal :inherit outline-4)
   (gnus-group-mail-5                            :foreground nil :weight normal :inherit outline-5)
   (gnus-group-mail-6                            :foreground nil :weight normal :inherit outline-6)
   (gnus-group-mail-1-empty                      :foreground ,base04 :inherit gnus-group-mail-1)
   (gnus-group-mail-2-empty                      :foreground ,base04 :inherit gnus-group-mail-2)
   (gnus-group-mail-3-empty                      :foreground ,base04 :inherit gnus-group-mail-3)
   (gnus-group-mail-4-empty                      :foreground ,base04 :inherit gnus-group-mail-4)
   (gnus-group-mail-5-empty                      :foreground ,base04 :inherit gnus-group-mail-5)
   (gnus-group-mail-6-empty                      :foreground ,base04 :inherit gnus-group-mail-6)
   (gnus-group-news-1                            :foreground nil :weight normal :inherit outline-5)
   (gnus-group-news-2                            :foreground nil :weight normal :inherit outline-6)
   (gnus-group-news-3                            :foreground nil :weight normal :inherit outline-7)
   (gnus-group-news-4                            :foreground nil :weight normal :inherit outline-8)
   (gnus-group-news-5                            :foreground nil :weight normal :inherit outline-1)
   (gnus-group-news-6                            :foreground nil :weight normal :inherit outline-2)
   (gnus-group-news-1-empty                      :foreground ,base04 :inherit gnus-group-news-1)
   (gnus-group-news-2-empty                      :foreground ,base04 :inherit gnus-group-news-2)
   (gnus-group-news-3-empty                      :foreground ,base04 :inherit gnus-group-news-3)
   (gnus-group-news-4-empty                      :foreground ,base04 :inherit gnus-group-news-4)
   (gnus-group-news-5-empty                      :foreground ,base04 :inherit gnus-group-news-5)
   (gnus-group-news-6-empty                      :foreground ,base04 :inherit gnus-group-news-6)

   ;; go-guru
   (go-guru-hl-identifier-face                   :background ,base02)

   ;; grep
   (grep-context-face                            :foreground ,base04)
   (grep-error-face                              :foreground ,base08 :weight bold :underline t)
   (grep-hit-face                                :foreground ,base0D)
   (grep-match-face                              :foreground nil :background nil :inherit match)

   ;; helm
   (helm-M-x-key                                 :foreground ,base0C)
   (helm-action                                  :foreground ,base05)
   (helm-buffer-directory                        :foreground ,base04 :background nil :weight bold)
   (helm-buffer-file                             :foreground ,base0C)
   (helm-buffer-not-saved                        :foreground ,base08)
   (helm-buffer-process                          :foreground ,base03)
   (helm-buffer-saved-out                        :foreground ,base0F)
   (helm-buffer-size                             :foreground ,base09)
   (helm-candidate-number                        :foreground ,base00 :background ,base09)
   (helm-ff-directory                            :foreground ,base04 :background nil :weight bold)
   (helm-ff-executable                           :foreground ,base0B)
   (helm-ff-file                                 :foreground ,base0C)
   (helm-ff-invalid-symlink                      :foreground ,base00 :background ,base08)
   (helm-ff-prefix                               :foreground nil :background nil)
   (helm-ff-symlink                              :foreground ,base00 :background ,base0C)
   (helm-grep-cmd-line                           :foreground ,base0B)
   (helm-grep-file                               :foreground ,base0C)
   (helm-grep-finish                             :foreground ,base00 :background ,base09)
   (helm-grep-lineno                             :foreground ,base03)
   (helm-grep-match                              :foreground ,base0A)
   (helm-grep-running                            :foreground ,base09)
   (helm-header                                  :foreground ,base0A :background ,base00 :underline nil)
   (helm-match                                   :foreground ,base0A)
   (helm-moccur-buffer                           :foreground ,base0C)
   (helm-selection                               :foreground nil :background ,base02 :underline nil)
   (helm-selection-line                          :foreground nil :background ,base02)
   (helm-separator                               :foreground ,base02)
   (helm-source-header                           :foreground ,base05 :background ,base01 :weight bold)
   (helm-visible-mark                            :foreground ,base00 :background ,base0B)

   ;; highlight-indentation minor mode
   (highlight-indentation-face                   :background ,base01)

   ;; highlight-thing mode
   (highlight-thing                              :inherit highlight)

   ;; hl-line-mode
   (hl-line                                      :background ,base01)
   (col-highlight                                :background ,base01)

   ;; hl-sexp-mode
   (hl-sexp-face                                 :background ,base03)

   ;; hydra
   (hydra-face-red                               :foreground ,base09)
   (hydra-face-blue                              :foreground ,base0D)

   ;; ido-mode
   (ido-subdir                                   :foreground ,base04)
   (ido-first-match                              :foreground ,base09 :weight bold)
   (ido-only-match                               :foreground ,base08 :weight bold)
   (ido-indicator                                :foreground ,base08 :background ,base01)
   (ido-virtual                                  :foreground ,base04)

   ;; idris-mode
   (idris-semantic-bound-face                    :inherit font-lock-variable-name-face)
   (idris-semantic-data-face                     :inherit font-lock-string-face)
   (idris-semantic-function-face                 :inherit font-lock-function-name-face)
   (idris-semantic-namespace-face                nil)
   (idris-semantic-postulate-face                :inherit font-lock-builtin-face)
   (idris-semantic-type-face                     :inherit font-lock-type-face)
   (idris-active-term-face                       :inherit highlight)
   (idris-colon-face                             :inherit font-lock-keyword-face)
   (idris-equals-face                            :inherit font-lock-keyword-face)
   (idris-operator-face                          :inherit font-lock-keyword-face)

   ;; imenu-list
   (imenu-list-entry-face-0                      :foreground ,base0A)
   (imenu-list-entry-face-1                      :foreground ,base0B)
   (imenu-list-entry-face-2                      :foreground ,base0D)
   (imenu-list-entry-face-3                      :foreground ,base0F)

   ;; ivy-mode
   (ivy-current-match                            :foreground ,base09 :background ,base01)
   (ivy-minibuffer-match-face-1                  :foreground ,base0E)
   (ivy-minibuffer-match-face-2                  :foreground ,base0D)
   (ivy-minibuffer-match-face-3                  :foreground ,base0C)
   (ivy-minibuffer-match-face-4                  :foreground ,base0B)
   (ivy-confirm-face                             :foreground ,base0B)
   (ivy-match-required-face                      :foreground ,base08)
   (ivy-virtual                                  :foreground ,base04)
   (ivy-action                                   :foreground ,base0D)

   ;; jabber
   (jabber-chat-prompt-local                     :foreground ,base0A)
   (jabber-chat-prompt-foreign                   :foreground ,base09)
   (jabber-chat-prompt-system                    :foreground ,base0A :weight bold)
   (jabber-chat-text-local                       :foreground ,base0A)
   (jabber-chat-text-foreign                     :foreground ,base09)
   (jabber-chat-text-error                       :foreground ,base08)

   (jabber-roster-user-online                    :foreground ,base0B)
   (jabber-roster-user-xa                        :foreground ,base04)
   (jabber-roster-user-dnd                       :foreground ,base0A)
   (jabber-roster-user-away                      :foreground ,base09)
   (jabber-roster-user-chatty                    :foreground ,base0E)
   (jabber-roster-user-error                     :foreground ,base08)
   (jabber-roster-user-offline                   :foreground ,base04)

   (jabber-rare-time-face                        :foreground ,base04)
   (jabber-activity-face                         :foreground ,base0E)
   (jabber-activity-personal-face                :foreground ,base0C)

   ;; js2-mode
   (js2-warning-face                             :underline ,base09)
   (js2-error-face                               :foreground nil :underline ,base08)
   (js2-external-variable-face                   :foreground ,base0E)
   (js2-function-param-face                      :foreground ,base0D)
   (js2-instance-member-face                     :foreground ,base0D)
   (js2-private-function-call-face               :foreground ,base08)

   ;; js3-mode
   (js3-warning-face                             :underline ,base09)
   (js3-error-face                               :foreground nil :underline ,base08)
   (js3-external-variable-face                   :foreground ,base0E)
   (js3-function-param-face                      :foreground ,base0D)
   (js3-jsdoc-tag-face                           :foreground ,base09)
   (js3-jsdoc-type-face                          :foreground ,base0C)
   (js3-jsdoc-value-face                         :foreground ,base0A)
   (js3-jsdoc-html-tag-name-face                 :foreground ,base0D)
   (js3-jsdoc-html-tag-delimiter-face            :foreground ,base0B)
   (js3-instance-member-face                     :foreground ,base0D)
   (js3-private-function-call-face               :foreground ,base08)

   ;; linum-mode
   (linum                                        :foreground ,base03 :background ,base01)

   ;; lsp-ui-doc
   (lsp-ui-doc-header                            :inherit org-document-title)
   (lsp-ui-doc-background                        :background ,base01)

   ;; lui-mode
   (lui-button-face                              :foreground ,base0D)
   (lui-highlight-face                           :background ,base01)
   (lui-time-stamp-face                          :foreground ,base0C)

   ;; magit
   (magit-blame-culprit                          :background ,base01)
   (magit-blame-heading                          :background ,base01 :foreground ,base05)
   (magit-branch                                 :foreground ,base04 :weight bold)
   (magit-branch-current                         :foreground ,base0C :weight bold :box t)
   (magit-branch-local                           :foreground ,base0C :weight bold)
   (magit-branch-remote                          :foreground ,base0B :weight bold)
   (magit-cherry-equivalent                      :foreground ,base0E)
   (magit-cherry-unmatched                       :foreground ,base0C)
   (magit-diff-context-highlight                 :background ,base01 :foreground ,base05)
   (magit-diff-file-header                       :background ,base01 :foreground ,base05)
   (magit-hash                                   :foreground ,base0D)
   (magit-header-line                            :background ,base02 :foreground ,base05 :weight bold)
   (magit-hunk-heading                           :background ,base03)
   (magit-hunk-heading-highlight                 :background ,base03)
   (magit-diff-hunk-heading                      :background ,base01)
   (magit-diff-hunk-heading-highlight            :background ,base01)
   (magit-item-highlight                         :background ,base01)
   (magit-log-author                             :foreground ,base0D)
   (magit-process-ng                             :foreground ,base08 :inherit magit-section-heading)
   (magit-process-ok                             :foreground ,base0B :inherit magit-section-heading)
   (magit-reflog-amend                           :foreground ,base0E)
   (magit-reflog-checkout                        :foreground ,base0D)
   (magit-reflog-cherry-pick                     :foreground ,base0B)
   (magit-reflog-commit                          :foreground ,base0B)
   (magit-reflog-merge                           :foreground ,base0B)
   (magit-reflog-other                           :foreground ,base0C)
   (magit-reflog-rebase                          :foreground ,base0E)
   (magit-reflog-remote                          :foreground ,base0C)
   (magit-reflog-reset                           :foreground ,base08)
   (magit-section-highlight                      :background ,base01)
   (magit-signature-bad                          :foreground ,base08 :weight bold)
   (magit-signature-error                        :foreground ,base08)
   (magit-signature-expired                      :foreground ,base09)
   (magit-signature-good                         :foreground ,base0B)
   (magit-signature-revoked                      :foreground ,base0E)
   (magit-signature-untrusted                    :foreground ,base0C)
   (magit-tag                                    :foreground ,base05)
   ;; mark-multiple
   (mm/master-face                               :foreground nil :background nil :inherit region)
   (mm/mirror-face                               :foreground nil :background nil :inherit region)

   ;; markdown-mode
   (markdown-url-face                            :inherit link)
   (markdown-link-face                           :foreground ,base0D :underline t)

   ;; message-mode
   (message-header-other                         :foreground nil :background nil :weight normal)
   (message-header-subject                       :foreground ,base0A :weight bold :inherit message-header-other)
   (message-header-to                            :foreground ,base09 :weight bold :inherit message-header-other)
   (message-header-cc                            :foreground nil :inherit message-header-to)
   (message-header-name                          :foreground ,base0D :background nil)
   (message-header-newsgroups                    :foreground ,base0C :background nil :slant normal)
   (message-separator                            :foreground ,base0E)

   ;; mic-paren
   (paren-face-match                             :foreground nil :background nil :inherit show-paren-match)
   (paren-face-mismatch                          :foreground nil :background nil :inherit show-paren-mismatch)
   (paren-face-no-match                          :foreground nil :background nil :inherit show-paren-mismatch)

   ;; mmm-mode
   (mmm-code-submode-face                        :background ,base03)
   (mmm-comment-submode-face                     :inherit font-lock-comment-face)
   (mmm-output-submode-face                      :background ,base03)

   ;; nxml-mode
   (nxml-name-face                               :foreground unspecified :inherit font-lock-constant-face)
   (nxml-attribute-local-name-face               :foreground unspecified :inherit font-lock-variable-name-face)
   (nxml-ref-face                                :foreground unspecified :inherit font-lock-preprocessor-face)
   (nxml-delimiter-face                          :foreground unspecified :inherit font-lock-keyword-face)
   (nxml-delimited-data-face                     :foreground unspecified :inherit font-lock-string-face)
   (rng-error-face                               :underline ,base08)

   ;; org-mode
   (org-agenda-structure                         :foreground ,base0E)
   (org-agenda-date                              :foreground ,base0D :underline nil)
   (org-agenda-done                              :foreground ,base0B)
   (org-agenda-dimmed-todo-face                  :foreground ,base04)
   (org-block                                    :foreground ,base09)
   (org-code                                     :foreground ,base0A)
   (org-column                                   :background ,base01)
   (org-column-title                             :weight bold :underline t :inherit org-column)
   (org-date                                     :foreground ,base0E :underline t)
   (org-document-info                            :foreground ,base0C)
   (org-document-info-keyword                    :foreground ,base0B)
   (org-document-title                           :foreground ,base09 :weight bold :height 1.44)
   (org-done                                     :foreground ,base0B :background ,base01)
   (org-ellipsis                                 :foreground ,base04)
   (org-footnote                                 :foreground ,base0C)
   (org-formula                                  :foreground ,base08)
   (org-hide                                     :foreground ,base03)
   (org-link                                     :foreground ,base0D)
   (org-scheduled                                :foreground ,base0B)
   (org-scheduled-previously                     :foreground ,base09)
   (org-scheduled-today                          :foreground ,base0B)
   (org-special-keyword                          :foreground ,base09)
   (org-table                                    :foreground ,base0E)
   (org-todo                                     :foreground ,base08 :background ,base01)
   (org-upcoming-deadline                        :foreground ,base09)
   (org-warning                                  :foreground ,base08 :weight bold)

   ;; paren-face-mode
   (paren-face                                   :foreground ,base04 :background nil)

   ;; popup
   (popup-face                                   :foreground ,base05 :background ,base02)
   (popup-isearch-match                          :foreground ,base00 :background ,base0B)
   (popup-scroll-bar-background-face             :background ,base03)
   (popup-scroll-bar-foreground-face             :background ,base05)
   (popup-summary-face                           :foreground ,base04)
   (popup-tip-face                               :foreground ,base00 :background ,base0A)
   (popup-menu-mouse-face                        :foreground ,base00 :background ,base0D)
   (popup-menu-selection-face                    :foreground ,base00 :background ,base0C)

   ;; powerline
   (powerline-active1                            :foreground ,base09 :background ,base00)
   (powerline-active2                            :foreground ,base08 :background ,base01)
   (powerline-inactive1                          :foreground ,base06 :background ,base01)
   (powerline-inactive2                          :foreground ,base07 :background ,base02)

   ;; python-mode
   (py-builtins-face                             :foreground ,base09 :weight normal)

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face              :foreground ,base0E)
   (rainbow-delimiters-depth-2-face              :foreground ,base0D)
   (rainbow-delimiters-depth-3-face              :foreground ,base0C)
   (rainbow-delimiters-depth-4-face              :foreground ,base0B)
   (rainbow-delimiters-depth-5-face              :foreground ,base0A)
   (rainbow-delimiters-depth-6-face              :foreground ,base09)
   (rainbow-delimiters-depth-7-face              :foreground ,base08)
   (rainbow-delimiters-depth-8-face              :foreground ,base03)
   (rainbow-delimiters-depth-9-face              :foreground ,base05)

   ;; regex-tool
   (regex-tool-matched-face                      :foreground nil :background nil :inherit match)

   ;; rhtml-mode
   (erb-delim-face                               :background ,base03)
   (erb-exec-face                                :background ,base03 :weight bold)
   (erb-exec-delim-face                          :background ,base03)
   (erb-out-face                                 :background ,base03 :weight bold)
   (erb-out-delim-face                           :background ,base03)
   (erb-comment-face                             :background ,base03 :weight bold :slant italic)
   (erb-comment-delim-face                       :background ,base03)

   ;; sh-mode
   (sh-heredoc                                   :foreground nil :weight normal :inherit font-lock-string-face)
   (sh-quoted-exec                               :foreground nil :inherit font-lock-preprocessor-face)

   ;; show-paren-mode
   (show-paren-match                             :foreground ,base01 :background ,base0D)
   (show-paren-mismatch                          :foreground ,base01 :background ,base09)

   ;; slime-mode
   (slime-highlight-edits-face                   :weight bold)
   (slime-repl-input-face                        :weight normal :underline nil)
   (slime-repl-prompt-face                       :foreground ,base0E :underline nil :weight bold)
   (slime-repl-result-face                       :foreground ,base0B)
   (slime-repl-output-face                       :foreground ,base0D :background ,base01)

   ;; smart-mode-line
   (sml/charging                                 :inherit sml/global :foreground ,base0B)
   (sml/discharging                              :inherit sml/global :foreground ,base08)
   (sml/filename                                 :inherit sml/global :foreground ,base0A :weight bold)
   (sml/global                                   :foreground ,base05)
   (sml/modes                                    :inherit sml/global :foreground ,base07)
   (sml/modified                                 :inherit sml/not-modified :foreground ,base08 :weight bold)
   (sml/outside-modified                         :inherit sml/not-modified :background ,base08)
   (sml/prefix                                   :inherit sml/global :foreground ,base09)
   (sml/read-only                                :inherit sml/not-modified :foreground ,base0C)

   ;; spaceline
   (spaceline-evil-emacs                         :foreground ,base01 :background ,base0D)
   (spaceline-evil-insert                        :foreground ,base01 :background ,base0D)
   (spaceline-evil-motion                        :foreground ,base01 :background ,base0E)
   (spaceline-evil-normal                        :foreground ,base01 :background ,base0B)
   (spaceline-evil-replace                       :foreground ,base01 :background ,base08)
   (spaceline-evil-visual                        :foreground ,base01 :background ,base09)

   ;; spacemacs
   (spacemacs-emacs-face                        :foreground ,base01 :background ,base0D)
   (spacemacs-hybrid-face                       :foreground ,base01 :background ,base0D)
   (spacemacs-insert-face                       :foreground ,base01 :background ,base0C)
   (spacemacs-motion-face                       :foreground ,base01 :background ,base0E)
   (spacemacs-lisp-face                         :foreground ,base01 :background ,base0E)
   (spacemacs-normal-face                       :foreground ,base01 :background ,base0B)
   (spacemacs-replace-face                      :foreground ,base01 :background ,base08)
   (spacemacs-visual-face                       :foreground ,base01 :background ,base09)

   ;; structured-haskell-mode
   (shm-current-face                             :inherit region)
   (shm-quarantine-face                          :underline (:style wave :color ,base08))

   ;; telephone-line
   (telephone-line-accent-active                 :foreground ,base00 :background ,base05)
   (telephone-line-accent-inactive               :foreground ,base01 :background ,base03)
   (telephone-line-evil-normal                   :foreground ,base01 :background ,base0B :weight bold)
   (telephone-line-evil-insert                   :foreground ,base01 :background ,base0D :weight bold)
   (telephone-line-evil-visual                   :foreground ,base06 :background ,base0E :weight bold)
   (telephone-line-evil-replace                  :foreground ,base01 :background ,base08 :weight bold)
   (telephone-line-evil-operator                 :foreground ,base0B :background ,base01 :weight bold)
   (telephone-line-evil-motion                   :foreground ,base00 :background ,base0C :weight bold)
   (telephone-line-evil-emacs                    :foreground ,base07 :background ,base0E :weight bold)
   (telephone-line-warning                       :foreground ,base09 :weight bold)
   (telephone-line-error                         :foreground ,base08 :weight bold)

   ;; term and ansi-term
   (term                                         :foreground ,base05 :background ,base00)
   (term-color-black                             :foreground ,base02 :background ,base00)
   (term-color-white                             :foreground ,base05 :background ,base07)
   (term-color-red                               :foreground ,base08 :background ,base08)
   (term-color-yellow                            :foreground ,base0A :background ,base0A)
   (term-color-green                             :foreground ,base0B :background ,base0B)
   (term-color-cyan                              :foreground ,base0C :background ,base0C)
   (term-color-blue                              :foreground ,base0D :background ,base0D)
   (term-color-magenta                           :foreground ,base0E :background ,base0E)

   ;; tooltip
   (tooltip                                      :background ,base01 :inherit default)

   ;; tuareg-mode
   (tuareg-font-lock-governing-face              :weight bold :inherit font-lock-keyword-face)

   ;; undo-tree-mode
   (undo-tree-visualizer-default-face            :foreground ,base06)
   (undo-tree-visualizer-current-face            :foreground ,base0B :weight bold)
   (undo-tree-visualizer-active-branch-face      :foreground ,base08)
   (undo-tree-visualizer-register-face           :foreground ,base0A)

   ;; utop-mode
   (utop-prompt                                  :foreground ,base0E)
   (utop-error                                   :underline (:style wave :color ,base08) :inherit error)

   ;; w3m-mode
   (w3m-anchor                                   :underline nil :inherit link)
   (w3m-anchor-visited                           :underline nil :inherit link-visited)
   (w3m-form                                     :foreground ,base09 :underline t)
   (w3m-image                                    :foreground ,base05 :background ,base03)
   (w3m-image-anchor                             :foreground ,base05 :background ,base03 :underline t)
   (w3m-header-line-location-content             :foreground ,base0D :background ,base00)
   (w3m-header-line-location-title               :foreground ,base0D :background ,base00)
   (w3m-tab-background                           :foreground ,base05 :background ,base01)
   (w3m-tab-selected                             :foreground ,base05 :background ,base00)
   (w3m-tab-selected-retrieving                  :foreground ,base05 :background ,base00)
   (w3m-tab-unselected                           :foreground ,base03 :background ,base01)
   (w3m-tab-unselected-unseen                    :foreground ,base03 :background ,base01)
   (w3m-tab-unselected-retrieving                :foreground ,base03 :background ,base01)

   ;; which-func-mode
   (which-func                                   :foreground ,base0D :background nil :weight bold)

   ;; whitespace-mode
   (whitespace-empty                             :foreground ,base08 :background ,base0A)
   (whitespace-hspace                            :foreground ,base04 :background ,base04)
   (whitespace-indentation                       :foreground ,base08 :background ,base0A)
   (whitespace-line                              :foreground ,base0F :background ,base01)
   (whitespace-newline                           :foreground ,base04)
   (whitespace-space                             :foreground ,base03 :background ,base01)
   (whitespace-space-after-tab                   :foreground ,base08 :background ,base0A)
   (whitespace-space-before-tab                  :foreground ,base08 :background ,base09)
   (whitespace-tab                               :foreground ,base03 :background ,base01)
   (whitespace-trailing                          :foreground ,base0A :background ,base08)))

(provide-theme 'base16)

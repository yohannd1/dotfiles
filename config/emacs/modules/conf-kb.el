;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun my/find-file-at (path)
  (let ((default-directory path))
    (call-interactively #'find-file)))

(defun my/find-file-at-home ()
  (interactive)
  (my/find-file-at "~"))

(defun my/find-file-at-config ()
  (interactive)
  (my/find-file-at user-modules-directory))

(setq
 conf-kb/keybindings
 `(("fh" ,#'my/find-file-at-home)
   ("fc" ,#'my/find-file-at-config)
   ("f." ,#'find-file)
   ("fr" ,#'counsel-recentf)
   ("fh" ,#'apropos)

   ("K" ,#'save-buffers-kill-emacs)
   ("e" ,#'eval-expression)
   ("E" ,#'eval-last-sexp)

   ("hv" ,#'describe-variable)
   ("hk" ,#'describe-key)
   ("hf" ,#'describe-function)
   ("hF" ,#'describe-face)

   ("rr" ,#'core-rifle-run)
   ("rb" ,#'core-rifle-build)
   ("rt" ,#'core-rifle-test)
   ("rc" ,#'core-rifle-check)

   ("m" ,#'counsel-M-x)
   (":" ,#'counsel-M-x)
   ("ç" ,#'counsel-M-x)

   ("w" ,#'whitespace-mode)
   ("s" ,#'vr/replace) ;; TODO: find something better than this

   ("bf" ,#'format-all-buffer)
   ("bs" ,#'counsel-switch-buffer)
   ("bo" ,#'counsel-switch-buffer-other-window)
   ("bk" ,#'kill-buffer)

   ("o" ,#'ace-window)))
(dolist (p conf-kb/keybindings)
  (define-key conf-kb/leader-map (nth 0 p) (nth 1 p)))

;; TODO: implement names for this shit (i spent around a hour on this and it didn't work)
(setq
 conf-kb/clipboard-bindings
 `(("p" ,#'evil-paste-after my/clip-paste-after)
   ("P" ,#'evil-paste-before my/clip-paste-before)
   ("y" ,#'evil-yank my/clip-yank)
   ("Y" ,#'evil-yank-line my/clip-yank-line)
   ("d" ,#'evil-delete my/clip-delete)
   ("D" ,#'evil-delete-line my/clip-delete-line)))

(cl-loop
 for (key function name) in conf-kb/clipboard-bindings do
 (define-key
  conf-kb/leader-map key
  `(lambda ()
     (interactive)
     (evil-use-register ?+)
     (call-interactively #',function))))

;; aliases for : on evil-mode
(dolist (x '("ç" "§" "Ã"))
  (define-key evil-motion-state-map x #'evil-ex))

(define-key evil-normal-state-map (kbd "C-j") #'evil-next-buffer)
(define-key evil-normal-state-map (kbd "C-k") #'evil-prev-buffer)
(define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)

;; reinforce leader key on dired
(define-key dired-mode-map (kbd "SPC") conf-kb/leader-map)

(provide 'conf-kb)

;; Remap ESC to cancelling commands
(define-key key-translation-map (kbd "ESC") (kbd "C-g")) ;; translation (currently testing to see if it works well)
(define-key key-translation-map (kbd "C-g") nil) ;; I don't want the pinky problem
(global-set-key [remap keyboard-quit] #'ice-escape)
(if (display-graphic-p) ;; on isearch
    (define-key isearch-mode-map [escape] 'isearch-abort)
  (define-key isearch-mode-map "\e" 'isearch-abort))

(defvar evil-leader-map (make-sparse-keymap)
  "A keymap with common functions.")

;; Switch to the leader map
(define-key evil-motion-state-map (kbd "SPC") evil-leader-map)

;; General commands
(define-key evil-motion-state-map (kbd "ç") #'evil-ex)
(define-key evil-motion-state-map (kbd "M-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "M-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "M-l") #'evil-window-right)
(define-key evil-motion-state-map (kbd "M-h") #'evil-window-left)
(define-key evil-leader-map (kbd ".") #'find-file)
(define-key evil-leader-map (kbd "e") #'eval-expression)
(define-key evil-leader-map (kbd "d") #'dired)

;; Insert mode
(define-key evil-insert-state-map (kbd "C-y") #'evil-paste-after)

;; Rifle commands
(define-key evil-leader-map (kbd "rr") #'ice-rifle-run)

;; Config commands
(define-key evil-leader-map (kbd "cr") #'ice-config-reload)
(define-key evil-leader-map (kbd "ce") #'(lambda ()
                                           (interactive)
                                           (find-file (f-join user-config-directory "main.el"))))

;; Etc.
(define-key evil-leader-map (kbd "s") #'vr/replace)
(define-key evil-leader-map (kbd "f") #'format-all-buffer)
(define-key global-map (kbd "C-j") #'centaur-tabs-forward)
(define-key global-map (kbd "C-k") #'centaur-tabs-backward)
(define-key evil-motion-state-map (kbd "C-j") #'centaur-tabs-forward)
(define-key evil-motion-state-map (kbd "C-k") #'centaur-tabs-backward)

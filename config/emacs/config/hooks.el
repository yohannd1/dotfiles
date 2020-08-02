;; Handle theme loading on clients.
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (ice-style-update)))))

;; Handle ice-tty cursor changing on terminals
(add-hook 'ice-style-update-pre-hook #'style-options)
(add-hook 'ice-style-update-hook #'centaur-tabs-headline-match)
(add-hook 'evil-normal-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-motion-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-replace-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-visual-state-entry-hook #'ice-tty-change-cursor)
(add-hook 'evil-insert-state-entry-hook (lambda () (ice-tty-change-cursor 6)))
(add-hook 'evil-operator-state-entry-hook (lambda () (ice-tty-change-cursor 3)))
(add-hook 'evil-emacs-state-entry-hook #'ice-tty-change-cursor)

(add-hook 'sh-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil
                          tab-width 2)))

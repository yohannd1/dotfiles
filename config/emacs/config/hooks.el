;; Handle theme loading on clients.
(when (daemonp)
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (with-selected-frame frame
		(ice--style-update)))))

;; Handle terminal cursor changin
(unless (display-graphic-p)
  (add-hook 'evil-normal-state-entry-hook #'terminal-change-cursor)
  (add-hook 'evil-motion-state-entry-hook #'terminal-change-cursor)
  (add-hook 'evil-replace-state-entry-hook #'terminal-change-cursor)
  (add-hook 'evil-visual-state-entry-hook #'terminal-change-cursor)
  (add-hook 'evil-insert-state-entry-hook (lambda () (terminal-change-cursor 6)))
  (add-hook 'evil-operator-state-entry-hook (lambda () (terminal-change-cursor 3)))
  (add-hook 'evil-emacs-state-entry-hook #'terminal-change-cursor))

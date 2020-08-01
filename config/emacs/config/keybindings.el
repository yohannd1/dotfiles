(defun wk-describe (key-string description)
  "A simple alias to `which-key-add-based-replacements'."
  (which-key-add-key-based-replacements key-string description))

(defun map (map key-string action &optional which-key-description) ;; TODO: flesh out this function
  (define-key map (kbd key-string) action)
  (when which-key-description
    (wk-describe key-string which-key-description)))

(defun map-dired (key directory)
  "Maps (kbd (concat \"SPC d\" KEY)) to opening dired on DIRECTORY."
  (map evil-motion-state-map
       (concat "SPC d" key) #'(lambda ()
				(interactive)
				(dired directory))
       (format "dired: %s" directory)))

(with-eval-after-load 'evil-maps
  ;; General commands
  (map evil-motion-state-map "ç" #'evil-ex)
  (map evil-motion-state-map "M-j" #'evil-window-down)
  (map evil-motion-state-map "M-k" #'evil-window-up)
  (map evil-motion-state-map "M-l" #'evil-window-right)
  (map evil-motion-state-map "M-h" #'evil-window-left)
  (map evil-normal-state-map "SPC ." #'helm-find-files)
  (map evil-normal-state-map "SPC e" #'eval-expression)

  ;; Insert mode
  (map evil-insert-state-map "C-y" #'evil-paste-after)

  ;; Rifle commands
  (wk-describe "SPC r" "rifle")
  (map evil-normal-state-map "SPC rr" #'ice-rifle-run)

  ;; Dired commands
  ;; (wk-describe "SPC d" "dired")
  ;; (map-dired "c" user-config-directory)
  ;; (map-dired "." ".")

  ;; Config commands
  (wk-describe "SPC c" "config")
  (map evil-normal-state-map "SPC cr" #'ice-config-reload)
  (map evil-normal-state-map "SPC ce" #'(lambda ()
					  (interactive)
					  (find-file (f-join user-config-directory "main.el"))) "edit main.el")
  (map evil-normal-state-map "SPC s" #'vr/replace)
  (map evil-normal-state-map "SPC f" #'format-all-buffer)
  (map global-map "C-j" #'centaur-tabs-forward)
  (map global-map "C-k" #'centaur-tabs-backward)
  (map evil-motion-state-map "C-j" #'centaur-tabs-forward)
  (map evil-motion-state-map "C-k" #'centaur-tabs-backward))

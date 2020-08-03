;; Ice Rifle
;; A collection of functions for testing, compiling and running files.
;;
;; TODO: make `rifle-override-hook', with hooks that run after geting the type of the file.
;; TODO: makefile, gradle and cargo thingies for this hook.
;; TODO: support for three rifle modes: silent, popup and buffer-term

(defun ice-rifle (command &optional mode)
  "Runs the `rifle-run' shell command, as [rifle-run COMMAND]."
    (start-process "rifle" "*Rifle (Background)*"
		   TERMINAL "-e" "runread" "rifle-run" command
		   (ice--get-name-for-mode (or mode major-mode))
           (buffer-file-name)))

(defun ice--get-name-for-mode (mode)
  "Gets the rifle name for the major mode `mode'.
Returns a string of `mode' without the \"-mode\" postfix as a fallback."
  (pcase mode
    (fallback (replace-regexp-in-string "-mode$" "" (symbol-name fallback)))))

(defun ice-rifle-run ()
  "A simple alias. Runs (ice-rifle \"run\")."
  (interactive)
  (ice-rifle "run"))

(provide 'ice-rifle)

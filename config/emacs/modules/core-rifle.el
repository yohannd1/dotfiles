;;; -*- lexical-binding: t; -*-
;;; Ice Rifle
;;; A collection of functions for testing, compiling and running files.
;;;
;;; TODO: make `rifle-override-hook', with hooks that run after geting the type of the file.
;;; TODO: makefile, gradle and cargo thingies for this hook.
;;; TODO: support for three rifle modes: silent, popup and buffer-term

(defun core-rifle (command &optional mode)
  "Runs the `rifle-run' shell command, as [rifle-run COMMAND]."
    (start-process "rifle" "*Rifle (Background)*"
		   TERMINAL "-e" "runread" "rifle-run" command
		   (core--get-name-for-mode (or mode major-mode))
           (buffer-file-name)))

(defun core--get-name-for-mode (mode)
  "Gets the rifle name for the major mode `mode'.
Returns a string of `mode' without the \"-mode\" postfix as a fallback."
  (cond
   ((file-upwards-parent "Makefile") "@make")
   ((file-upwards-parent "makefile") "@make")
   ((file-upwards-parent "Cargo.toml") "@cargo")
   (t (pcase mode
        ('c++-mode "cpp")
        ('mhtml-mode "html")
        (fallback (replace-regexp-in-string "-mode$" "" (symbol-name fallback)))))))

(defun core-rifle-run ()
  (interactive)
  (core-rifle "run"))

(defun core-rifle-build ()
  (interactive)
  (core-rifle "build"))

(defun core-rifle-test ()
  (interactive)
  (core-rifle "test"))

(defun core-rifle-check ()
  (interactive)
  (core-rifle "check"))

(provide 'core-rifle)

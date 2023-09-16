;; -*- lexical-binding: t -*-

;; Show colours in compilation buffer
(setq compilation-environment '("TERM=xterm-256color"))
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)


;; Auto close compile buffer if no errors
(setq compilation-finish-functions
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            ;;no errors, make the compilation window go away in a few seconds
            (progn
              (run-at-time "0.6 sec" nil
                           (lambda ()
                             (select-window (get-buffer-window (get-buffer-create "*compilation*")))
                             (quit-window)))
              (message "No Compilation Errors!")))))

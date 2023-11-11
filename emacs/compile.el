;; -*- lexical-binding: t -*-

;; Show colours in compilation buffer
(setq compilation-environment '("TERM=xterm-256color"))
(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun hym/close-compilation-buffer ()
  "Finds and closes the compilation buffer"
  (interactive)
  (let ((compilation-buffer (get-buffer-window (get-buffer-create "*compilation*"))))
    (if compilation-buffer
        (progn
          (select-window compilation-buffer)
          (quit-window)))))

(evil-define-key 'normal 'global
  (kbd "<escape>") 'hym/close-compilation-buffer)

;; -*- lexical-binding: t -*-

;; Inspired by https://www.masteringemacs.org/article/demystifying-emacs-window-manager

(setq switch-to-buffer-obey-display-actions t)

(defun hym/split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

(defun hym/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))

(setq display-buffer-alist
      `(
        (,(rx "*Compilation*")
         . (display-buffer-reuse-window))

        ;; Always open eshell in new window
        (,(rx anychar "eshell*")
          . ((display-buffer-reuse-window display-buffer-pop-up-window)
             (inhibit-same-window . t)))

        ))

(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window))

(defun hym/ace-window-prefix ()
  "Use `ace-window' to display the buffer of the next command.
The next buffer is the buffer displayed by the next command invoked
immediately after this command (ignoring reading from the minibuffer).
Creates a new window before displaying the buffer.
When `switch-to-buffer-obey-display-actions' is non-nil,
`switch-to-buffer' commands are also supported."
  (interactive)
  (display-buffer-override-next-command
   (lambda (buffer _)
     (let (window type)
       (setq
        window (aw-select (propertize " ACE" 'face 'mode-line-highlight))
        type 'reuse)
       (cons window type)))
   nil "[ace-window]")
  (message "Use `ace-window' to display next command buffer..."))

(keymap-global-set "C-x 4 o" 'hym/ace-window-prefix)

(tab-bar-history-mode 1)
(hym/leader-def
  "wu" 'tab-bar-history-back
  "wr" 'tab-bar-history-forward
  "ws" 'window-toggle-side-windows)

;; TODO configure strokes mode
;; (global-set-key (kbd "<down-mouse-9>") 'strokes-do-stroke)

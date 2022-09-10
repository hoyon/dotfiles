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

        ;; Open info and man in right sidebar
        (,(rx (or "*info*" (seq "*man " (* anychar) "*") (seq "*WoMan " (* anychar) "*")))
         . ((display-buffer-in-side-window)
            (side . right)
            (slot . 0)
            (window-width . 80)
            (window-parameters
             (no-delete-other-windows . t))))

        ;; Reuse open help buffers
        (,(rx (or "*Help*" "*helpful"))
              . ((display-buffer-reuse-window display-buffer-pop-up-window)
                 (inhibit-same-window . t)))

        ;; Always open eshell in new window
        (,(rx anychar "eshell*")
          . ((display-buffer-reuse-window display-buffer-pop-up-window)
             (inhibit-same-window . t)))

        ))

(winner-mode 1)
(hym/leader-def
  "wu" 'winner-undo
  "wr" 'winner-redo
  "ws" 'window-toggle-side-windows)

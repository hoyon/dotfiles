;; -*- lexical-binding: t -*-

(setq history-delete-duplicates t
      eshell-hist-ignoredups t
      eshell-history-size 1024)

(defun hym/eshell-clear ()
  (interactive)
  (eshell/clear-scrollback) (eshell-send-input))

(defun hym/eshell-kill-before-point ()
  (interactive)
  (kill-line 0))

(general-define-key
 :states 'insert
 :keymaps 'eshell-mode-map
 "C-a" 'eshell-bol
 "C-e" 'eshell-show-maximum-output
 "C-l" 'hym/eshell-clear
 "C-u" 'eshell-kill-input
 "C-p" 'eshell-previous-input
 "C-n" 'eshell-next-input
 "C-d" 'eshell-life-is-too-much)

(defun hym/eshell-mode-hook ()
  (setq show-trailing-whitespace nil))

(add-hook 'eshell-mode-hook 'hym/eshell-mode-hook)

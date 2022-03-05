;; -*- lexical-binding: t -*-

;; eshell config
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

;; vterm config
(use-package vterm
  :config
  (setq vterm-shell "/bin/fish")

  (general-define-key
   :states 'insert
   :keymaps 'vterm-mode-map
   "C-a" 'vterm-send-C-a
   "C-e" 'vterm-send-C-e
   "C-l" 'vterm-send-C-l
   "C-u" 'vterm-send-C-u
   "C-p" 'vterm-send-C-p
   "C-n" 'vterm-send-C-p
   "C-d" 'vterm-send-C-d)

  (general-define-key
   :states 'normal
   :keymaps 'vterm-mode-map
   "p" 'vterm-yank))


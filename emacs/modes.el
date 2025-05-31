;; -*- lexical-binding: t -*-

;; Dired

(setq
 dired-listing-switches "-alh" ;; Show human sizes in dired mode
 dired-dwim-target t ;; Guess target directory when copying and renaming files
 )

(use-package dired-du)

;; Tramp

;; Make tramp work properly when using fish shell
(setq shell-file-name "/bin/bash")

;; Make tramp work with guix
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; Disable some tramp messages
(setq-default tramp-verbose 1)

(setq tramp-default-method "ssh")

(setq vc-handled-backends '(Git))
(setq tramp-ssh-controlmaster-options "")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq remote-file-name-inhibit-locks t)

(defun my-magit-auto-revert-mode-advice (orig-fun &rest args)
  (unless (and buffer-file-name (file-remote-p buffer-file-name))
    (apply orig-fun args)))

(advice-add 'magit-turn-on-auto-revert-mode-if-desired
            :around
            #'my-magit-auto-revert-mode-advice)

;; Misc modes

(save-place-mode 1)

(use-package writeroom-mode
  :config
  (setq writeroom-width 100))

(use-package yasnippet
  :delight (yas-minor-mode)
  :config
  (yas-global-mode 1))

(use-package info+)
(use-package scratch)
(use-package restclient)

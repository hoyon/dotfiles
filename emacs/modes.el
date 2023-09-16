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
(setq-default tramp-verbose 2)


;; Misc modes

(save-place-mode 1)

(use-package writeroom-mode
  :config
  (setq writeroom-width 100))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package info+)
(use-package scratch)
(use-package restclient)

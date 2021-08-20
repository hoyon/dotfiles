;; -*- lexical-binding: t -*-

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-C-u-scroll t)
  :config
  (evil-mode 1)
  (evil-define-key 'normal 'global
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line
    "u" 'undo-fu-only-undo
    "\C-r" 'undo-fu-only-redo)
  ;; :custom
  ;; (evil-search-module 'isearch)
  )

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(magit dired help helpful info))
  (evil-collection-init))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-define-key 'normal 'global
    "gcc" 'evilnc-comment-or-uncomment-lines
    "gcp" 'evilnc-comment-or-uncomment-paragraphs
    "gct" 'evilnc-comment-or-uncomment-html-tag
    "gcd" 'evilnc-copy-and-comment-lines))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

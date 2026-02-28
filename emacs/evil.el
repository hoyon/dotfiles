;; -*- lexical-binding: t -*-

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-fu
        evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t)
  :config
  (evil-mode 1)
  (general-define-key
   :states 'normal
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line
   "u" 'undo-fu-only-undo
   "C-r" 'undo-fu-only-redo)

  ;; Use § key as escape key for when using ipad magic keyboard
  (general-define-key
   :states '(insert visual)
   "§" 'evil-normal-state)

  ;; allow '#' entry on Mac UK keyboard
  (general-define-key
   :states 'insert
   "M-3" (lambda () (interactive) (insert "#")))

  ;; :custom
  ;; (evil-search-module 'isearch)

  ;; make evil-search-word look for symbol rather than word boundaries
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t)

  ;; use hippie-expand instead of dabbrev
  (general-define-key [remap dabbrev-expand] 'hippie-expand)
  (setq evil-complete-next-func 'hippie-expand))

(use-package evil-matchit
  :after evil
  :config
  (setq evilmi-always-simple-jump t)
  (global-evil-matchit-mode 1))

(use-package evil-collection
  :after evil
  :delight (evil-collection-unimpaired-mode)
  :config
  (setq evil-collection-mode-list
        '(calc
          cider
          compile
          corfu
          dired
          forge
          grep
          help
          helpful
          ibuffer
          info
          magit
          man
          org
          restclient
          tab-bar
          vterm
          wgrep
          woman))
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (general-define-key
   :states 'normal
   "gcc" 'evilnc-comment-or-uncomment-lines
   "gcp" 'evilnc-comment-or-uncomment-paragraphs
   "gct" 'evilnc-comment-or-uncomment-html-tag
   "gcd" 'evilnc-copy-and-comment-lines))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(general-define-key
 :states 'normal
 :keymaps 'messages-buffer-mode-map
 "q" 'quit-window)

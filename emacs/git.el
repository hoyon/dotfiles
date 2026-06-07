;; -*- lexical-binding: t -*-

(use-package magit
  :config
  (add-hook 'magit-status-sections-hook #'magit-insert-worktrees t)
  (hym/leader-def
    "gg" 'magit-status
    "gb" 'magit-blame))

(use-package forge
  :after magit)

(setq smerge-command-prefix "C-c v")

(use-package git-timemachine
  :straight
  (:host github :repo "emacsmirror/git-timemachine")
  :config
  (hym/leader-def
    "gt" 'git-timemachine)
  (general-define-key
   :states 'normal
   :keymaps 'git-timemachine-mode-map
   "p" 'git-timemachine-show-previous-revision
   "n" 'git-timemachine-show-next-revision
   "q" 'git-timemachine-quit
   "g" 'git-timemachine-show-nth-revision
   "t" 'git-timemachine-show-revision-fuzzy
   "b" 'git-timemachine-blame
   "c" 'git-timemachine-show-commit))

(use-package git-link
  :config
  (setq
   git-link-open-in-browser 't
   git-link-use-single-line-number nil)

  (hym/leader-def
    "go" 'git-link
    "gr" 'git-link-homepage))

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode)
  (text-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (setq diff-hl-draw-borders nil)

  ;; Live update instead of only on save
  (diff-hl-flydiff-mode)

  ;; Make the fringe narrower
  (fringe-mode '(4 . 8))
  (set-face-attribute 'fringe nil :background nil))

(use-package majutsu
  :straight (:host github :repo "0WD0/majutsu"))

(use-package vc-jj)

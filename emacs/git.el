;; -*- lexical-binding: t -*-

(use-package magit
  :config
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  (hym/leader-def
    "gg" 'magit-status
    "gb" 'magit-blame))

(use-package forge
  :after magit)

(setq smerge-command-prefix "C-c v")

(use-package git-timemachine
  :config
  (hym/leader-def
    "gt" 'git-timemachine)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "p" 'git-timemachine-show-previous-revision
    "n" 'git-timemachine-show-next-revision
    "q" 'git-timemachine-quit
    "g" 'git-timemachine-show-nth-revision
    "t" 'git-timemachine-show-revision-fuzzy
    "b" 'git-timemachine-blame
    "c" 'git-timemachine-show-commit))

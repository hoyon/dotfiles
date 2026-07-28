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
  (diff-hl-mode . hym/diff-hl-set-branch-reference)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (defun hym/diff-hl-branch-reference-revision ()
    "Return the merge-base revision for branch-wide `diff-hl' markers."
    (when-let* ((root (ignore-errors (magit-toplevel)))
                (default-directory root)
                (main-branch (magit-main-branch)))
      (magit-git-string "merge-base" main-branch "HEAD")))

  (defun hym/diff-hl-set-branch-reference ()
    "Show `diff-hl' markers for all changes on the current Git branch."
    (when (and buffer-file-name
               (eq (vc-backend buffer-file-name) 'Git))
      (if-let ((revision (hym/diff-hl-branch-reference-revision)))
          (setq-local diff-hl-reference-revision revision)
        (kill-local-variable 'diff-hl-reference-revision))))

  (setq
   vc-git-diff-switches '("-U0")
   diff-hl-draw-borders nil)

  (defvar hym/diff-hl--refresh-timer nil)

  (defun hym/diff-hl-refresh-selected-buffer (&rest _)
    "Refresh `diff-hl' in the selected window's buffer, after a short idle.
Updates otherwise only happen on save and revert, so a commit or branch
switch made outside Emacs would leave the markers stale until the next
save.  Rescheduling rather than stacking timers keeps rapid buffer
switching down to a single refresh of wherever you land."
    (let ((buf (window-buffer (selected-window))))
      (when (buffer-local-value 'diff-hl-mode buf)
        (when (timerp hym/diff-hl--refresh-timer)
          (cancel-timer hym/diff-hl--refresh-timer))
        (setq hym/diff-hl--refresh-timer
              (run-with-idle-timer 0.5 nil #'diff-hl--update-buffer buf)))))

  (add-hook 'window-buffer-change-functions
            #'hym/diff-hl-refresh-selected-buffer)
  (add-hook 'window-selection-change-functions
            #'hym/diff-hl-refresh-selected-buffer)

  (advice-add 'diff-hl-update :before #'hym/diff-hl-set-branch-reference)

  ;; Make the fringe narrower
  (fringe-mode '(4 . 8))
  (set-face-attribute 'fringe nil :background nil))

(use-package majutsu
  :straight (:host github :repo "0WD0/majutsu"))

(use-package vc-jj)

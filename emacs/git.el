;; -*- lexical-binding: t -*-

(use-package magit
  :config
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
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
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

;; Use delta to show side by side diffs of staged and unstaged changes

(defvar-local hym/git-delta-diff--args nil)
(defvar-local hym/git-delta-diff--directory nil)

(defun hym/git-delta-diff-refresh ()
  "Refresh the current delta diff buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (default-directory hym/git-delta-diff--directory)
        (pos (point)))
    (erase-buffer)
    (call-process-shell-command
     (format "GIT_PAGER=cat git diff -U5 %s | delta --side-by-side --width %d"
             (or hym/git-delta-diff--args "")
             (- (frame-width) 1))
     nil t)
    (if (= (point-min) (point-max))
        (insert "No changes")
      (ansi-color-apply-on-region (point-min) (point-max)))
    (goto-char (min pos (point-max)))))

(defun hym/git-delta-diff--show-buffer (buf)
  "Display BUF in a tab, reusing existing tab if present."
  (if-let ((tab (tab-bar-get-buffer-tab buf t t)))
      (progn
        (tab-bar-switch-to-tab (alist-get 'name tab))
        (switch-to-buffer buf))
    (switch-to-buffer-other-tab buf)))

(defun hym/git-delta-diff (&optional args)
  "Show git diff through delta side-by-side in a buffer."
  (interactive)
  (unless (executable-find "delta")
    (user-error "delta not found in PATH"))
  (let* ((dir (magit-toplevel))
         (default-directory dir)
         (diff-type (if (string= args "--cached") "staged" "unstaged"))
         (buf (get-buffer-create (format "*delta-diff[%s]: %s*" diff-type (project-name (project-current))))))
    (with-current-buffer buf
      (setq-local hym/git-delta-diff--args args)
      (setq-local hym/git-delta-diff--directory dir)
      (hym/git-delta-diff-refresh)
      (goto-char (point-min))
      (special-mode)
      (evil-local-set-key 'normal "q" 'tab-close)
      (evil-local-set-key 'normal "gr" 'hym/git-delta-diff-refresh))
    (hym/git-delta-diff--show-buffer buf)))

(defun hym/git-delta-diff-staged ()
  "Show staged diff through delta side-by-side."
  (interactive)
  (hym/git-delta-diff "--cached"))

(defun hym/git-delta-diff-unstaged ()
  "Show unstaged diff through delta side-by-side."
  (interactive)
  (hym/git-delta-diff))

(hym/leader-def
  "gd" 'hym/git-delta-diff-staged
  "gD" 'hym/git-delta-diff-unstaged)

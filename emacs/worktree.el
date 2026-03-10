;; -*- lexical-binding: t -*-

;; Git worktree integration for Emacs

;;; Core worktree detection

(defun hym/worktree-list ()
  "Return list of (path branch) for all worktrees in current repo.
Returns nil if not in a git repo."
  (when-let* ((default-directory (or (vc-git-root default-directory)
                                      default-directory))
              (output (shell-command-to-string "git worktree list --porcelain 2>/dev/null")))
    (let (worktrees current-path current-branch)
      (dolist (line (split-string output "\n"))
        (cond
         ((string-prefix-p "worktree " line)
          (when current-path
            (push (list current-path current-branch) worktrees))
          (setq current-path (substring line 9)
                current-branch nil))
         ((string-prefix-p "branch " line)
          (setq current-branch (file-name-nondirectory (substring line 7))))))
      (when current-path
        (push (list current-path current-branch) worktrees))
      (nreverse worktrees))))

(defun hym/worktree-name (worktree-path)
  "Extract a short name from WORKTREE-PATH.
Uses the directory name as the worktree name."
  (file-name-nondirectory (directory-file-name worktree-path)))

;;; Cross-worktree file finder

(defun hym/worktree-select ()
  "Select a worktree using completing-read.
Returns (path branch) or nil."
  (when-let ((worktrees (hym/worktree-list)))
    (let* ((candidates (mapcar (lambda (wt)
                                 (cons (format "%s (%s)"
                                               (hym/worktree-name (car wt))
                                               (or (cadr wt) "detached"))
                                       wt))
                               worktrees))
           (selection (completing-read "Worktree: " candidates nil t)))
      (cdr (assoc selection candidates)))))

(defun hym/worktree-find-file ()
  "Find file in a selected worktree."
  (interactive)
  (when-let ((wt (hym/worktree-select)))
    (let ((default-directory (car wt)))
      (call-interactively #'project-find-file))))

(defun hym/worktree-switch ()
  "Open dired in a selected worktree."
  (interactive)
  (when-let ((wt (hym/worktree-select)))
    (dired (car wt))))

(defun hym/worktree-magit-status ()
  "Open magit-status in a selected worktree."
  (interactive)
  (when-let ((wt (hym/worktree-select)))
    (magit-status (car wt))))

;;; Worktree deletion

(defun hym/worktree--non-main ()
  "Return worktree list excluding the main worktree."
  (cdr (hym/worktree-list)))

(defun hym/worktree--remove (path)
  "Remove worktree at PATH, offering force delete on failure."
  (message "Deleting worktree: %s" path)
  (if (= 0 (call-process "git" nil nil nil "worktree" "remove" path))
      (message "Deleted worktree: %s" path)
    (let ((diff (string-trim (shell-command-to-string
                               (format "git -C %s diff --stat HEAD" (shell-quote-argument path)))))
          (untracked (string-trim (shell-command-to-string
                                    (format "git -C %s ls-files --others --exclude-standard" (shell-quote-argument path))))))
      (when (yes-or-no-p
             (format "Delete failed.\n%s%s\nForce delete %s?"
                     (if (string-empty-p diff) "" (concat diff "\n"))
                     (if (string-empty-p untracked) ""
                       (concat (mapconcat (lambda (f) (concat " ?? " f))
                                          (split-string untracked "\n" t) "\n")
                               "\n"))
                     path))
        (if (= 0 (call-process "git" nil nil nil "worktree" "remove" "--force" path))
            (message "Force deleted worktree: %s" path)
          (message "Failed to force delete: %s" path))))))

(defun hym/worktree-delete ()
  "Delete a git worktree, selected via completing-read."
  (interactive)
  (let* ((current-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel 2>/dev/null")))
         (secondaries (hym/worktree--non-main))
         (candidates (mapcar (lambda (wt)
                               (let* ((path (car wt))
                                      (name (hym/worktree-name path))
                                      (current-p (string= path current-root))
                                      (label (format "%s (%s)%s"
                                                     name
                                                     (or (cadr wt) "detached")
                                                     (if current-p " (current)" ""))))
                                 (cons label path)))
                             secondaries))
         (selection (completing-read "Delete worktree: " candidates nil t))
         (path (cdr (assoc selection candidates))))
    (when path
      (when (string= path current-root)
        (setq default-directory (car (car (hym/worktree-list)))))
      (hym/worktree--remove path))))

;;; Project list cleanup

(defun hym/worktree-prune-projects ()
  "Remove deleted worktrees from the project list."
  (interactive)
  (let ((before-count (length project--list))
        (pruned 0))
    (setq project--list
          (seq-filter (lambda (entry)
                        (let ((path (car entry)))
                          (if (file-directory-p path)
                              t
                            (setq pruned (1+ pruned))
                            (message "Pruned missing project: %s" path)
                            nil)))
                      project--list))
    (when (> pruned 0)
      (project--write-project-list)
      (message "Pruned %d missing projects" pruned))))

;;; Keybindings

(with-eval-after-load 'general
  (hym/leader-def
    "gw" '(:ignore t :which-key "worktree")
    "gww" 'hym/worktree-switch
    "gwf" 'hym/worktree-find-file
    "gwg" 'hym/worktree-magit-status
    "gwd" 'hym/worktree-delete
    "gwp" 'hym/worktree-prune-projects))

(provide 'worktree)

;; -*- lexical-binding: t -*-
(require 'seq)

(defun hym-workspace--pick-repo (ws)
  "Return an absolute repo directory chosen from WS.
Auto-returns the sole repo; prompts over repo names when several. For
project/notes workspaces `:repos' is (\".\"), so this returns the root."
  (let* ((repos (hym-workspace-repos ws))
         (repo (cond ((null repos) (user-error "No repos in this workspace"))
                     ((null (cdr repos)) (car repos))
                     (t (completing-read "Repo: " repos nil t)))))
    (file-name-as-directory (expand-file-name repo (hym-workspace-root ws)))))

(defun hym-workspace-git-status ()
  "Open magit status for a repo in the current workspace."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (magit-status (hym-workspace--pick-repo ws))))

(defun hym-workspace-git-diff ()
  "Show the PR-style delta diff (merge-base..HEAD) for a workspace repo."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (let ((default-directory (hym-workspace--pick-repo ws)))
      (hym/git-delta-diff-merge-base))))

(defun hym-workspace-git-diff-unstaged-with-untracked ()
  "Show unstaged diff, including untracked files, for a workspace repo."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (let ((default-directory (hym-workspace--pick-repo ws)))
      (hym/git-delta-diff-unstaged-with-untracked))))

(defun hym-workspace-git-log ()
  "Open magit log for a repo in the current workspace.
Press D on a commit (or over a selected range) for its delta diff."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (let ((default-directory (hym-workspace--pick-repo ws)))
      (magit-log-head))))

(provide 'hym-workspaces-git)

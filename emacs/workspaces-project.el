;; -*- lexical-binding: t -*-

(require 'project)
(require 'seq)

(defun hym/workspace-at (dir)
  "Return the multi-repo workspace whose root contains DIR, or nil.
Only worktree workspaces count: every other type's root is a single repo,
which project.el already scopes correctly."
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (seq-find (lambda (ws)
                (and (eq (hym/workspace-type ws) 'worktree)
                     (string-prefix-p (file-name-as-directory (hym/workspace-root ws))
                                      dir)))
              (hym/workspace-registry))))

(defun hym/workspace--project (ws)
  (cons 'hym/workspace (file-name-as-directory (hym/workspace-root ws))))

(cl-defmethod project-root ((project (head hym/workspace)))
  (cdr project))

;; The workspace root is not a git repo but each sub-repo is, so fd applies
;; every sub-repo's .gitignore while still listing the root's loose files.
;; Symlinks are not followed: the root's .claude/.agents links point back
;; into the sub-repos and would duplicate their files.
(cl-defmethod project-files ((project (head hym/workspace)) &optional dirs)
  (let ((dirs (or dirs (list (project-root project)))))
    (mapcan
     (lambda (dir)
       (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
              (files (process-lines "fd" "--type" "f" "--hidden" "--exclude" ".git"
                                    "--color" "never")))
         (if (and project-files-relative-names (length= dirs 1))
             files
           (mapcar (lambda (f) (expand-file-name f default-directory)) files))))
     dirs)))

(defun hym/workspace-find-file (&optional include-all)
  "Find a file across the current workspace, or the current project outside one.
With prefix argument INCLUDE-ALL, include gitignored files."
  (interactive "P")
  (if-let* ((ws (hym/workspace-at default-directory)))
      (let* ((project (hym/workspace--project ws))
             (project-files-relative-names t))
        (project-find-file-in (thing-at-point 'filename)
                              (list (project-root project))
                              project include-all))
    (project-find-file include-all)))

(defun hym/project-root (&optional may-prompt)
  "Return the current project's root, prompting for one when MAY-PROMPT."
  (when-let* ((project (project-current may-prompt)))
    (project-root project)))

(defun hym/workspace-search-root (&optional may-prompt)
  "Return the current workspace's root, or the project root outside one."
  (if-let* ((ws (hym/workspace-at default-directory)))
      (project-root (hym/workspace--project ws))
    (hym/project-root may-prompt)))

(setq consult-project-function #'hym/workspace-search-root)

(defun hym/project-ripgrep ()
  "Ripgrep the current repo only, even inside a multi-repo workspace."
  (interactive)
  (let ((consult-project-function #'hym/project-root))
    (consult-ripgrep)))

(provide 'hym/workspaces-project)

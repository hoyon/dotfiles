;; -*- lexical-binding: t -*-
(require 'seq)

(defcustom hym-workspace-code-root "~/code"
  "Directory holding canonical repositories to make worktrees from."
  :type 'directory :group 'hym-workspace)

(defun hym-workspace--repo-conductor (repo-dir)
  "Return the `scripts' alist from REPO-DIR's conductor.json, or nil."
  (let ((file (expand-file-name "conductor.json" repo-dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (alist-get 'scripts (json-parse-buffer :object-type 'alist
                                               :null-object nil))))))

(defun hym-workspace--available-repos ()
  "Return names of git repos under `hym-workspace-code-root'.
A conductor.json is optional; without one, provisioning just does the
worktree add and skips the setup/archive steps."
  (let ((root (expand-file-name hym-workspace-code-root)))
    (when (file-directory-p root)
      (seq-filter
       (lambda (name)
         (file-exists-p (expand-file-name (concat name "/.git") root)))
       (seq-remove (lambda (n) (string-prefix-p "." n))
                   (directory-files root nil))))))

(defun hym-workspace--provision-command (ws repo reuse-branch)
  "Return the shell command that provisions REPO for WS.
Adds the worktree (creating branch `:slug' unless REUSE-BRANCH) and, when
the repo has a conductor.json `setup', runs it with the conductor env vars."
  (let* ((slug (hym-workspace-slug ws))
         (code (expand-file-name repo (expand-file-name hym-workspace-code-root)))
         (dest (expand-file-name repo (hym-workspace-root ws)))
         (base (hym-workspace-base-branch ws))
         (setup (alist-get 'setup (hym-workspace--repo-conductor code)))
         (add (if reuse-branch
                  (format "git -C %s worktree add %s %s"
                          (shell-quote-argument code)
                          (shell-quote-argument dest)
                          (shell-quote-argument slug))
                (format "git -C %s worktree add -b %s %s %s"
                        (shell-quote-argument code)
                        (shell-quote-argument slug)
                        (shell-quote-argument dest)
                        (shell-quote-argument base)))))
    (if setup
        (format "%s && cd %s && CONDUCTOR_ROOT_PATH=%s CONDUCTOR_WORKSPACE_NAME=%s sh -c %s"
                add
                (shell-quote-argument dest)
                (shell-quote-argument code)
                (shell-quote-argument slug)
                (shell-quote-argument setup))
      add)))

(defun hym-workspace--default-run-async (name command buffer callback)
  "Run COMMAND (a shell string) async, streaming to BUFFER.
Call CALLBACK with t on zero exit, nil otherwise."
  (make-process
   :name name
   :buffer buffer
   :command (list shell-file-name shell-command-switch command)
   :connection-type 'pipe
   :sentinel (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (funcall callback (eq 0 (process-exit-status proc)))))))

(defvar hym-workspace--run-async #'hym-workspace--default-run-async
  "Function (NAME COMMAND BUFFER CALLBACK) running COMMAND asynchronously.
CALLBACK is called with t on success, nil on failure. Rebound in tests.")

(defvar hym-workspace--provisioning (make-hash-table :test 'equal)
  "Map workspace slug to `(:repo R :state running|failed)' during setup.
Runtime only; never persisted.")

(defun hym-workspace--refresh-sidebar ()
  (when (fboundp 'hym-workspace-sidebar-refresh)
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace--setup-buffer (ws)
  "Return WS's setup-output buffer, hidden (space-prefixed) until an error."
  (get-buffer-create (format " *ws-setup: %s*" (hym-workspace-slug ws))))

(defun hym-workspace--provision (ws repos reuse-branch &optional on-done)
  "Provision REPOS for WS sequentially through `hym-workspace--run-async'.
Call ON-DONE with t when all succeed, nil on the first failure."
  (let ((slug (hym-workspace-slug ws))
        (buffer (hym-workspace--setup-buffer ws)))
    (letrec ((step
              (lambda (remaining)
                (if (null remaining)
                    (progn
                      (remhash slug hym-workspace--provisioning)
                      (hym-workspace--refresh-sidebar)
                      (when on-done (funcall on-done t)))
                  (let ((repo (car remaining)))
                    (puthash slug (list :repo repo :state 'running)
                             hym-workspace--provisioning)
                    (hym-workspace--refresh-sidebar)
                    (funcall hym-workspace--run-async
                             (format "ws-setup-%s-%s" slug repo)
                             (hym-workspace--provision-command ws repo reuse-branch)
                             buffer
                             (lambda (ok)
                               (if ok
                                   (funcall step (cdr remaining))
                                 (puthash slug (list :repo repo :state 'failed)
                                          hym-workspace--provisioning)
                                 (hym-workspace--refresh-sidebar)
                                 (when on-done (funcall on-done nil))))))))))
      (funcall step repos))))

(defun hym-workspace--provisioning-badge (ws)
  "Status function: a badge line for WS while it is provisioning."
  (when-let ((st (gethash (hym-workspace-slug ws) hym-workspace--provisioning)))
    (list (pcase (plist-get st :state)
            ('running (propertize (format "~ provisioning %s..." (plist-get st :repo))
                                  'face 'warning))
            ('failed (propertize (format "! setup failed (%s)" (plist-get st :repo))
                                 'face 'error))
            ('archiving (propertize (format "~ archiving %s..." (plist-get st :repo))
                                    'face 'warning))
            ('archive-failed (propertize (format "! archive failed (%s)" (plist-get st :repo))
                                         'face 'error))))))

(with-eval-after-load 'hym-workspaces-sidebar
  (add-to-list 'hym-workspace-sidebar-status-functions
               #'hym-workspace--provisioning-badge))

(defun hym-workspace--register-worktree (name base-branch repos)
  "Validate slug uniqueness for NAME and register a worktree entry.
Return the workspace. Does not touch disk."
  (let* ((slug (hym-workspace--slugify name))
         (root (expand-file-name slug (expand-file-name hym-workspace-home))))
    (when (string-empty-p slug)
      (user-error "Workspace name has no usable characters"))
    (when (null repos)
      (user-error "Pick at least one repo"))
    (when (or (hym-workspace-get name)
              (seq-find (lambda (w) (equal (hym-workspace-slug w) slug))
                        (hym-workspace-registry))
              (file-exists-p root))
      (user-error "A workspace with slug %s already exists" slug))
    (hym-workspace-put (list :name name :slug slug :type 'worktree
                             :root (abbreviate-file-name root)
                             :repos repos :base-branch base-branch
                             :archived nil))))

(defun hym-workspace--show-setup-error (ws)
  "Reveal WS's setup output buffer, which is otherwise hidden."
  (display-buffer (hym-workspace--setup-buffer ws)))

(defun hym-workspace--read-repos (&optional exclude)
  "Read one or more repos interactively, EXCLUDE-ing some, one at a time.
Loops rather than using `completing-read-multiple' so any completion UI
can pick several. Finishes on the [done] sentinel."
  (let ((available (seq-difference (hym-workspace--available-repos) exclude))
        (chosen nil)
        (done nil))
    (while (and available (not done))
      (let ((pick (completing-read
                   (format "Repo (%d chosen, RET on [done] to finish): "
                           (length chosen))
                   (append available '("[done]")) nil t)))
        (if (member pick '("[done]" "" nil))
            (setq done t)
          (push pick chosen)
          (setq available (delete pick available)))))
    (nreverse chosen)))

(defun hym-workspace-create-worktree (name repos)
  "Create and provision a worktree workspace named NAME with REPOS.
The base branch is always main. Setup runs in a hidden buffer, revealed
only if it fails."
  (interactive (list (read-string "Workspace name: ")
                     (hym-workspace--read-repos)))
  (let ((ws (hym-workspace--register-worktree name "main" repos)))
    (make-directory (hym-workspace-root ws) t)
    (hym-workspace-open ws)
    (hym-workspace--provision
     ws repos nil
     (lambda (ok) (unless ok (hym-workspace--show-setup-error ws))))
    ws))

(defun hym-workspace-add-repo (ws repo)
  "Add REPO to worktree WS, provisioning it, and append to `:repos'."
  (interactive
   (let ((ws (or (hym-workspace-current) (user-error "Not in a workspace"))))
     (list ws (completing-read
               "Add repo: "
               (seq-difference (hym-workspace--available-repos)
                               (hym-workspace-repos ws))
               nil t))))
  (when (member repo (hym-workspace-repos ws))
    (user-error "%s is already in this workspace" repo))
  (hym-workspace--provision
   ws (list repo) nil
   (lambda (ok)
     (if ok
         (let ((cur (hym-workspace-get (hym-workspace-name ws))))
           (hym-workspace-put
            (plist-put (copy-sequence cur) :repos
                       (append (hym-workspace-repos cur) (list repo)))))
       (hym-workspace--show-setup-error ws)))))

(defun hym-workspace--archive-command (ws repo)
  "Return the shell command tearing REPO down: archive script then remove."
  (let* ((slug (hym-workspace-slug ws))
         (code (expand-file-name repo (expand-file-name hym-workspace-code-root)))
         (dest (expand-file-name repo (hym-workspace-root ws)))
         (archive (alist-get 'archive (hym-workspace--repo-conductor code)))
         (remove (format "git -C %s worktree remove --force %s"
                         (shell-quote-argument code)
                         (shell-quote-argument dest))))
    (if archive
        ;; Gate the worktree removal on the archive script succeeding, so a
        ;; failed archive (e.g. couldn't drop a DB) surfaces as archive-failed
        ;; rather than being swallowed and the workspace marked archived anyway.
        (format "cd %s && CONDUCTOR_ROOT_PATH=%s CONDUCTOR_WORKSPACE_NAME=%s sh -c %s && %s"
                (shell-quote-argument dest)
                (shell-quote-argument code)
                (shell-quote-argument slug)
                (shell-quote-argument archive)
                remove)
      remove)))

(defun hym-workspace-archive-worktree (ws)
  "Tear WS down to just its branch, marking it archived only when teardown
of every repo succeeds; surface failure via the provisioning badge."
  (hym-workspace-close ws)
  (let ((slug (hym-workspace-slug ws))
        (buffer (hym-workspace--setup-buffer ws)))
    (letrec ((step
              (lambda (remaining)
                (if (null remaining)
                    (progn
                      (remhash slug hym-workspace--provisioning)
                      (when-let ((cur (hym-workspace-get (hym-workspace-name ws))))
                        (hym-workspace-put (plist-put (copy-sequence cur) :archived t)))
                      (hym-workspace--refresh-sidebar))
                  (let ((repo (car remaining)))
                    (puthash slug (list :repo repo :state 'archiving)
                             hym-workspace--provisioning)
                    (hym-workspace--refresh-sidebar)
                    (funcall hym-workspace--run-async
                             (format "ws-archive-%s-%s" slug repo)
                             (hym-workspace--archive-command ws repo)
                             buffer
                             (lambda (ok)
                               (if ok
                                   (funcall step (cdr remaining))
                                 (puthash slug (list :repo repo :state 'archive-failed)
                                          hym-workspace--provisioning)
                                 (hym-workspace--refresh-sidebar)
                                 (hym-workspace--show-setup-error ws)
                                 (message "Archive failed for %s in %s"
                                          repo (hym-workspace-name ws))))))))))
      (funcall step (hym-workspace-repos ws)))))

(defun hym-workspace-unarchive (ws)
  "Un-archive WS and re-provision its repos onto the existing branch."
  (let ((active (hym-workspace-put (plist-put (copy-sequence ws) :archived nil))))
    (make-directory (hym-workspace-root active) t)
    (hym-workspace--provision
     active (hym-workspace-repos active) t
     (lambda (ok) (unless ok (hym-workspace--show-setup-error active))))))

(defun hym-workspace--repo-worktree-p (ws repo)
  "Non-nil when REPO already has a worktree checked out in WS."
  (file-exists-p (expand-file-name (concat repo "/.git") (hym-workspace-root ws))))

(defun hym-workspace-provision-retry (ws)
  "Clear WS's failed provisioning and re-provision any repos still missing.
Repos whose worktree already exists are left alone; a repo that failed
before its worktree was created is re-provisioned from scratch."
  (interactive (list (hym-workspace-current)))
  (remhash (hym-workspace-slug ws) hym-workspace--provisioning)
  (hym-workspace--refresh-sidebar)
  (let ((missing (seq-remove (lambda (repo) (hym-workspace--repo-worktree-p ws repo))
                             (hym-workspace-repos ws))))
    (when missing
      (make-directory (hym-workspace-root ws) t)
      (hym-workspace--provision
       ws missing nil
       (lambda (ok) (unless ok (hym-workspace--show-setup-error ws)))))))

(with-eval-after-load 'hym-workspaces
  (add-to-list 'hym-workspace-type-creators
               '(worktree . hym-workspace-create-worktree)))

(provide 'hym-workspaces-worktree)

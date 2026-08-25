;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defcustom hym-workspace-code-root "~/code"
  "Directory holding canonical repositories to make worktrees from."
  :type 'directory :group 'hym-workspace)

(defcustom hym-workspace-presets-file
  (expand-file-name "presets.eld" hym-workspace-home)
  "File defining worktree presets, kept outside the dotfiles repo.
A list of plists, e.g. (:name \"frontend\" :repos (\"ploy-client\")
:base-branch \"main\" :agent \"claude\")."
  :type 'file :group 'hym-workspace)

(defun hym-workspace-presets ()
  "Return the preset list from `hym-workspace-presets-file'.
A missing file yields nil; a present-but-unparseable file signals."
  (hym-workspace--read-eld hym-workspace-presets-file "presets file"))

(defun hym-workspace-preset-name (preset) (plist-get preset :name))
(defun hym-workspace-preset-repos (preset) (plist-get preset :repos))
(defun hym-workspace-preset-base-branch (preset)
  (or (plist-get preset :base-branch) "main"))
(defun hym-workspace-preset-agent (preset) (plist-get preset :agent))

(defun hym-workspace--name-taken-p (name)
  "Return non-nil when NAME or its derived slug already exists in the registry."
  (let ((slug (hym-workspace--slugify name)))
    (or (hym-workspace-get name)
        (seq-find (lambda (w) (equal (hym-workspace-slug w) slug))
                  (hym-workspace-registry)))))

(defun hym-workspace--unique-name (base)
  "Return BASE, or BASE with a numeric suffix, free of name/slug clashes."
  (if (not (hym-workspace--name-taken-p base))
      base
    (let ((n 2))
      (while (hym-workspace--name-taken-p (format "%s %d" base n))
        (setq n (1+ n)))
      (format "%s %d" base n))))

(defun hym-workspace--name-from-prompt (prompt)
  "Derive a unique workspace name from PROMPT's first few words."
  (let* ((words (seq-take (split-string (downcase prompt) "[^a-z0-9]+" t) 5))
         (base (string-join words " ")))
    (hym-workspace--unique-name (if (string-empty-p base) "workspace" base))))

(defun hym-workspace--code-dir (repo)
  "Return the canonical checkout of REPO under `hym-workspace-code-root'."
  (expand-file-name repo (expand-file-name hym-workspace-code-root)))

(defun hym-workspace--repo-conductor (repo-dir)
  "Return the `scripts' alist from REPO-DIR's conductor.json, or nil."
  (let ((file (expand-file-name "conductor.json" repo-dir)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (alist-get 'scripts (json-parse-buffer :object-type 'alist
                                               :null-object nil))))))

(defun hym-workspace--conductor-script (repo script)
  "Return REPO's conductor SCRIPT (a symbol such as `run'), or nil."
  (alist-get script (hym-workspace--repo-conductor
                     (hym-workspace--code-dir repo))))

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

(defun hym-workspace--git-string (directory &rest args)
  "Run git with ARGS in DIRECTORY, returning trimmed stdout on success."
  (when (file-directory-p directory)
    (with-temp-buffer
      (let ((default-directory directory))
        (when (= 0 (apply #'call-process "git" nil (list t nil) nil args))
          (string-trim (buffer-string)))))))

(defun hym-workspace--branch-start-point (code base)
  "Return (FETCH-REFSPEC . START-POINT) for branching off BASE in CODE.
FETCH-REFSPEC is nil when CODE has no origin to fetch from, in which case
the branch starts from the local BASE, or from HEAD when BASE is absent."
  (if (hym-workspace--git-string code "remote" "get-url" "origin")
      ;; Keep the source fully qualified.  With fetch.prune enabled, a
      ;; short source such as "main" is not matched against
      ;; "refs/heads/main" during pruning, so Git deletes origin/main
      ;; immediately before trying to update it.
      (cons (format "refs/heads/%s:refs/remotes/origin/%s" base base)
            (format "origin/%s" base))
    (cons nil
          (if (hym-workspace--git-string code "rev-parse" "--verify" "--quiet"
                                         (format "refs/heads/%s" base))
              base
            "HEAD"))))

(defun hym-workspace--repo-dest (ws repo)
  "Return REPO's checkout directory inside WS."
  (expand-file-name repo (hym-workspace-root ws)))

(defun hym-workspace--conductor-command (ws repo script)
  "Return the shell command running SCRIPT in REPO's checkout for WS.
The conductor environment names the canonical repo and the workspace."
  (format "cd %s && CONDUCTOR_ROOT_PATH=%s CONDUCTOR_WORKSPACE_NAME=%s sh -c %s"
          (shell-quote-argument (hym-workspace--repo-dest ws repo))
          (shell-quote-argument (hym-workspace--code-dir repo))
          (shell-quote-argument (hym-workspace-slug ws))
          (shell-quote-argument script)))

(defun hym-workspace--worktree-command (ws repo reuse-branch)
  "Return the shell command that adds REPO's worktree for WS.
Create branch `:slug' unless REUSE-BRANCH is non-nil."
  (let* ((slug (hym-workspace-slug ws))
         (code (hym-workspace--code-dir repo))
         (dest (hym-workspace--repo-dest ws repo))
         (base (hym-workspace-base-branch ws)))
    (if reuse-branch
        (format "git -C %s worktree add %s %s"
                (shell-quote-argument code)
                (shell-quote-argument dest)
                (shell-quote-argument slug))
      (pcase-let* ((`(,fetch-refspec . ,start-point)
                    (hym-workspace--branch-start-point code base))
                   (add (format "git -C %s worktree add -b %s %s %s"
                                (shell-quote-argument code)
                                (shell-quote-argument slug)
                                (shell-quote-argument dest)
                                (shell-quote-argument start-point))))
        (if fetch-refspec
            (format "git -C %s fetch origin %s && %s"
                    (shell-quote-argument code)
                    (shell-quote-argument fetch-refspec)
                    add)
          add)))))

(defun hym-workspace--setup-command (ws repo)
  "Return REPO's setup command for WS, or nil when none is configured."
  (when-let ((setup (hym-workspace--conductor-script repo 'setup)))
    (hym-workspace--conductor-command ws repo setup)))

(defun hym-workspace--provision-command (ws repo reuse-branch)
  "Return a command that adds REPO's worktree and runs its setup for WS.
Kept as a command-building helper; multi-repo provisioning runs these as
separate phases so all worktrees exist before any setup begins."
  (let ((add (hym-workspace--worktree-command ws repo reuse-branch))
        (setup (hym-workspace--setup-command ws repo)))
    (if setup (format "%s && %s" add setup) add)))

(defconst hym-workspace--claude-asset-dirs '("skills" "agents")
  "Subdirectories of `.claude' whose entries are surfaced per repo.")

(defun hym-workspace--claude-dir (root kind)
  (expand-file-name (concat ".claude/" kind) root))

(defun hym-workspace--prune-claude-links (ws)
  "Delete dangling `.claude' links at WS's root, left by removed worktrees."
  (dolist (kind hym-workspace--claude-asset-dirs)
    (let ((dir (hym-workspace--claude-dir (hym-workspace-root ws) kind)))
      (when (file-directory-p dir)
        (dolist (name (directory-files dir nil directory-files-no-dot-files-regexp))
          (let ((link (expand-file-name name dir)))
            (when (and (file-symlink-p link) (not (file-exists-p link)))
              (delete-file link))))))))

(defun hym-workspace--link-claude-assets (ws repo)
  "Link REPO's skills and agents into WS's root `.claude'.
An agent started at the workspace root only discovers skills and agents
under the root's own `.claude', so each repo's entries are linked in
individually.  Where two repos define the same name, the first repo in
`:repos' keeps it."
  (let ((root (hym-workspace-root ws)))
    (dolist (kind hym-workspace--claude-asset-dirs)
      (let ((source (hym-workspace--claude-dir
                     (expand-file-name repo root) kind)))
        (when (file-directory-p source)
          (let ((dest (hym-workspace--claude-dir root kind)))
            (make-directory dest t)
            (dolist (name (directory-files
                           source nil directory-files-no-dot-files-regexp))
              (let ((link (expand-file-name name dest)))
                (unless (file-exists-p link)
                  (when (file-symlink-p link) (delete-file link))
                  (make-symbolic-link (expand-file-name name source) link))))))))))

(defun hym-workspace--sync-claude-assets (ws repos)
  "Refresh WS's root `.claude' so it points at REPOS' skills and agents."
  (hym-workspace--prune-claude-links ws)
  (dolist (repo repos)
    (hym-workspace--link-claude-assets ws repo)))

(defun hym-workspace-link-claude-assets (ws)
  "Refresh WS's root `.claude' links, for workspaces provisioned before this."
  (interactive (list (or (hym-workspace-current) (user-error "Not in a workspace"))))
  (hym-workspace--sync-claude-assets ws (hym-workspace-repos ws)))

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

(defvar hym-workspace--jobs (make-hash-table :test 'equal)
  "Map workspace key to `(:repo R :state S)' while a repo job is in flight.
S is one of `running', `failed', `archiving' or `archive-failed'.
Runtime only; never persisted.")

(defun hym-workspace--set-job (ws repo state)
  "Record that WS is in STATE on REPO, and redraw."
  (puthash (hym-workspace--key ws) (list :repo repo :state state)
           hym-workspace--jobs)
  (hym-workspace-refresh-ui))

(defun hym-workspace--clear-job (ws)
  "Forget WS's in-flight job, and redraw."
  (remhash (hym-workspace--key ws) hym-workspace--jobs)
  (hym-workspace-refresh-ui))

(defun hym-workspace--setup-buffer (ws)
  "Return WS's setup-output buffer, hidden (space-prefixed) until an error."
  (get-buffer-create (format " *ws-setup: %s*" (hym-workspace--key ws))))

(defun hym-workspace--run-per-repo (ws repos command-fn state on-done)
  "Run COMMAND-FN's shell command for each of REPOS in turn, for WS.
Mark WS as STATE on the repo being worked on. A nil command skips that
repo. Call ON-DONE with t once every repo succeeds, or with the failing
repo's name on the first failure, and stop there."
  (let ((buffer (hym-workspace--setup-buffer ws))
        (key (hym-workspace--key ws)))
    (letrec ((step
              (lambda (remaining)
                (if (null remaining)
                    (funcall on-done t)
                  (let* ((repo (car remaining))
                         (command (funcall command-fn repo)))
                    (if (null command)
                        (funcall step (cdr remaining))
                      (hym-workspace--set-job ws repo state)
                      (funcall hym-workspace--run-async
                               (format "ws-%s-%s-%s" state key repo)
                               command buffer
                               (lambda (ok)
                                 (if ok
                                     (funcall step (cdr remaining))
                                   (funcall on-done repo))))))))))
      (funcall step repos))))

(defun hym-workspace--provision (ws repos reuse-branch &optional on-done)
  "Provision REPOS for WS through `hym-workspace--run-async'.
Add every worktree sequentially before running any configured setup scripts.
Call ON-DONE with t when all succeed, nil on the first failure."
  (let ((finish (lambda (result)
                  (if (eq result t)
                      (hym-workspace--clear-job ws)
                    (hym-workspace--set-job ws result 'failed))
                  (when on-done (funcall on-done (eq result t))))))
    (hym-workspace--run-per-repo
     ws repos
     (lambda (repo) (hym-workspace--worktree-command ws repo reuse-branch))
     'running
     (lambda (result)
       (if (not (eq result t))
           (funcall finish result)
         (hym-workspace--sync-claude-assets ws repos)
         (hym-workspace--run-per-repo
          ws repos
          (lambda (repo) (hym-workspace--setup-command ws repo))
          'running
          finish))))))

(defun hym-workspace--job-badge (ws)
  "Status function: a badge line for WS while a repo job is in flight."
  (when-let ((st (gethash (hym-workspace--key ws) hym-workspace--jobs)))
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
               #'hym-workspace--job-badge))

(defun hym-workspace--register-worktree (name base-branch repos)
  "Validate slug uniqueness for NAME and register a worktree entry.
Return the workspace. Does not touch disk."
  (let* ((slug (hym-workspace--slugify name))
         (root (expand-file-name slug (expand-file-name hym-workspace-home))))
    (when (string-empty-p slug)
      (user-error "Workspace name has no usable characters"))
    (when (null repos)
      (user-error "Pick at least one repo"))
    (when (or (hym-workspace--name-taken-p name) (file-exists-p root))
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

(defun hym-workspace--read-add-repo (ws)
  "Read a repo to add to WS, excluding the ones it already has."
  (completing-read "Add repo: "
                   (seq-difference (hym-workspace--available-repos)
                                   (hym-workspace-repos ws))
                   nil t))

(defun hym-workspace-add-repo (ws repo)
  "Add REPO to worktree WS, provisioning it, and append to `:repos'."
  (interactive
   (let ((ws (or (hym-workspace-current) (user-error "Not in a workspace"))))
     (list ws (hym-workspace--read-add-repo ws))))
  (when (member repo (hym-workspace-repos ws))
    (user-error "%s is already in this workspace" repo))
  (hym-workspace--provision
   ws (list repo) nil
   (lambda (ok)
     (if ok
         (when-let ((cur (hym-workspace-get (hym-workspace-name ws))))
           (hym-workspace-update
            cur :repos (append (hym-workspace-repos cur) (list repo))))
       (hym-workspace--show-setup-error ws)))))

(defun hym-workspace--archive-command (ws repo)
  "Return the shell command tearing REPO down: archive script then remove."
  (let ((archive (hym-workspace--conductor-script repo 'archive))
        (remove (format "git -C %s worktree remove --force %s"
                        (shell-quote-argument (hym-workspace--code-dir repo))
                        (shell-quote-argument
                         (hym-workspace--repo-dest ws repo)))))
    (if archive
        ;; Gate the worktree removal on the archive script succeeding, so a
        ;; failed archive (e.g. couldn't drop a DB) surfaces as archive-failed
        ;; rather than being swallowed and the workspace marked archived anyway.
        (format "%s && %s"
                (hym-workspace--conductor-command ws repo archive)
                remove)
      remove)))

(defun hym-workspace--repo-worktree-registered-p (ws repo)
  "Non-nil when REPO is still registered as a git worktree for WS."
  (let* ((code (expand-file-name repo (expand-file-name hym-workspace-code-root)))
         (dest (expand-file-name repo (hym-workspace-root ws)))
         (output (hym-workspace--git-string code "worktree" "list" "--porcelain")))
    (and output
         (seq-some
          (lambda (line)
            (and (string-prefix-p "worktree " line)
                 (string= (expand-file-name (substring line 9)) dest)))
          (split-string output "\n" t)))))

(defun hym-workspace--repo-worktree-archived-p (ws repo)
  "Non-nil when REPO's worktree for WS is already gone."
  (or (not (hym-workspace--repo-worktree-p ws repo))
      (not (hym-workspace--repo-worktree-registered-p ws repo))))

(defun hym-workspace--kill-buffers (ws)
  "Kill WS's buffers so their processes are reaped before its worktree goes.
Agent terminals are never registered as servers, and closing a tab group
only discards the window configuration, so nothing else here triggers the
`kill-buffer-hook' that terminals use to reap their subprocesses.  A
buffer visiting a file with unsaved changes is left open, since the
worktree is about to be removed and the buffer holds the only copy."
  (let ((root (file-name-as-directory (hym-workspace-root ws)))
        (kept 0))
    (dolist (buf (buffer-list))
      (when (buffer-live-p buf)
        (let ((dir (buffer-local-value 'default-directory buf)))
          ;; The minibuffer inherits `default-directory' from wherever it was
          ;; last summoned, so it matches the root without belonging to WS.
          (when (and dir
                     (not (minibufferp buf))
                     (string-prefix-p root (expand-file-name dir)))
            (if (and (buffer-file-name buf) (buffer-modified-p buf))
                (setq kept (1+ kept))
              ;; Bind rather than clear process flags: ghostel installs its
              ;; own query function that ignores `process-query-on-exit-flag'.
              (let ((kill-buffer-query-functions nil))
                (kill-buffer buf)))))))
    (when (> kept 0)
      (message "%s: left %d modified buffer(s) open"
               (hym-workspace-name ws) kept))))

(defun hym-workspace-archive-worktree (ws)
  "Tear WS down to just its branch, marking it archived only when teardown
of every repo succeeds; surface failure via the provisioning badge."
  (hym-workspace-run-teardown ws)
  (hym-workspace--kill-buffers ws)
  (hym-workspace-close ws)
  (hym-workspace--run-per-repo
   ws (hym-workspace-repos ws)
   (lambda (repo)
     (unless (hym-workspace--repo-worktree-archived-p ws repo)
       (hym-workspace--archive-command ws repo)))
   'archiving
   (lambda (result)
     (if (eq result t)
         (progn
           (hym-workspace--clear-job ws)
           (hym-workspace--prune-claude-links ws)
           (hym-workspace-update ws :archived t)
           (hym-workspace-refresh-ui))
       (hym-workspace--set-job ws result 'archive-failed)
       (hym-workspace--show-setup-error ws)
       (message "Archive failed for %s in %s"
                result (hym-workspace-name ws))))))

(defun hym-workspace-unarchive (ws)
  "Un-archive WS and re-provision its repos onto the existing branch."
  (let ((active (hym-workspace-update ws :archived nil)))
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
  (hym-workspace--clear-job ws)
  (let ((missing (seq-remove (lambda (repo) (hym-workspace--repo-worktree-p ws repo))
                             (hym-workspace-repos ws))))
    (when missing
      (make-directory (hym-workspace-root ws) t)
      (hym-workspace--provision
       ws missing nil
       (lambda (ok) (unless ok (hym-workspace--show-setup-error ws)))))))

(with-eval-after-load 'hym-workspaces
  (add-to-list 'hym-workspace-type-creators
               '(worktree . hym-workspace-create-worktree))
  (add-to-list 'hym-workspace-type-handlers
               (list 'worktree
                     :archive #'hym-workspace-archive-worktree
                     :unarchive #'hym-workspace-unarchive
                     :retry #'hym-workspace-provision-retry
                     :add-repo (lambda (ws)
                                 (hym-workspace-add-repo
                                  ws (hym-workspace--read-add-repo ws))))))

(provide 'hym-workspaces-worktree)

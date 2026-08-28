;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Declared special so our `let' binds them dynamically even before
;; ghostel-compile.el (autoloaded) defines `ghostel-compile-buffer-name';
;; without this the lexical-binding file binds it lexically and the later
;; defcustom errors with "Defining as dynamic an already lexical var".
(defvar ghostel-compile-buffer-name)
(defvar ghostel-environment)
(defvar agent-shell-buffer-name)
(declare-function agent-shell-new-shell "agent-shell")

(defcustom hym-workspace-agents '(("claude" . "claude")
                                  ("codex" . "codex")
                                  ("goose" . "goose"))
  "Alist of (NAME . SHELL-COMMAND) for agents launchable in a workspace."
  :type '(alist :key-type string :value-type string) :group 'hym-workspace)

(defcustom hym-workspace-server-environments-file
  (expand-file-name "server-environments.eld" hym-workspace-home)
  "File mapping repo names to environment variables for their servers.
The file contains an alist whose keys are repo names and whose values are
lists of \"KEY=VALUE\" strings.  It is read on every server start, so edits
take effect without restarting Emacs."
  :type 'file :group 'hym-workspace)

(defvar hym-workspace--servers (make-hash-table :test 'equal)
  "Map (WORKSPACE-KEY REPO) to its server (`ghostel-compile') buffer name.")

(defcustom hym-workspace-old-server-tab-prefix "old:"
  "Prefix added to server tabs after their process is retired."
  :type 'string :group 'hym-workspace)

(defcustom hym-workspace-restart-server-delay 0.05
  "Seconds to wait between starting servers during a bulk restart."
  :type 'number :group 'hym-workspace)

(defcustom hym-workspace-server-shutdown-timeout 3
  "Seconds to let a server exit after SIGTERM before sending SIGKILL."
  :type 'number :group 'hym-workspace)

(defvar hym-workspace--agent-state (make-hash-table :test 'equal)
  "Map (SLUG SESSION) to agent state plists.
Each value has keys :slug, :agent, :session, :state, and :updated-at.")

(defcustom hym-workspace-agent-working-timeout 300
  "Seconds after which an unrefreshed `working' agent state is considered stale.
Waiting and permission states do not expire automatically because they are
actionable and should stay visible until the agent reports a new state or the
agent buffer is killed."
  :type 'number :group 'hym-workspace)

(defcustom hym-workspace-agent-stale-check-interval 30
  "Seconds between automatic stale-agent checks."
  :type 'number :group 'hym-workspace)

(defvar hym-workspace--agent-stale-timer nil
  "Timer used to clear stale `working' agent states.")

(defun hym-workspace--ghostel-environment ()
  "Return the current Ghostel environment list, or nil before Ghostel loads."
  (and (boundp 'ghostel-environment) ghostel-environment))

(defvar-local hym-workspace--terminal-workspace nil
  "Name of the workspace whose tab spawned this terminal buffer.")

(defun hym-workspace--tag-terminal ()
  "Record the current workspace's name on this terminal buffer."
  (when-let* ((ws (hym-workspace-current)))
    (setq hym-workspace--terminal-workspace (hym-workspace-name ws))))

(add-hook 'ghostel-mode-hook #'hym-workspace--tag-terminal)

(defun hym-workspace--notification-text (title body)
  "Format an OSC notification as \"[WORKSPACE] TITLE: BODY\".
Drop the workspace when the buffer was not spawned in one, and the title
when the agent sent none (iTerm2-style OSC 9); with neither, fall back to
the buffer name so the message still says where it came from."
  (let* ((ws hym-workspace--terminal-workspace)
         (summary (cond ((and title (not (string-empty-p title))) title)
                        ((null ws) (buffer-name)))))
    (concat (and ws (format "[%s] " ws))
            (and summary (format "%s: " summary))
            body)))

(defun hym-workspace-notify (title body)
  "Echo an agent's OSC notification, prefixed with the workspace it came from.
Ghostel calls this with the originating terminal current, so the prefix is
right even when another workspace's tab is selected."
  (message "%s" (hym-workspace--notification-text title body)))

(setq ghostel-notification-function #'hym-workspace-notify)

(defun hym-workspace--server-environment (repo)
  "Return the configured server environment for REPO.
Read `hym-workspace-server-environments-file' on every call.  A missing file
or an unconfigured repo yields nil; malformed entries signal an error rather
than starting a server with an unexpected environment."
  (let ((entries
         (hym-workspace--read-eld hym-workspace-server-environments-file
                                  "server environments file")))
    (unless
        (and (listp entries)
             (seq-every-p
              (lambda (entry)
                (and (consp entry)
                     (stringp (car entry))
                     (listp (cdr entry))
                     (seq-every-p #'stringp (cdr entry))))
              entries))
      (error "Invalid server environments in %s"
             hym-workspace-server-environments-file))
    (alist-get repo entries nil nil #'equal)))

(defun hym-workspace--agent-state-key (slug session)
  "Return the hash key for SLUG and SESSION."
  (list slug session))

(defun hym-workspace--agent-clear-state (slug &optional session)
  "Clear tracked agent state for SLUG.
When SESSION is non-nil, clear only that session."
  (if session
      (remhash (hym-workspace--agent-state-key slug session)
               hym-workspace--agent-state)
    (let (keys)
      (maphash
       (lambda (key _entry)
         (when (equal slug (car-safe key))
           (push key keys)))
       hym-workspace--agent-state)
      (dolist (key keys)
        (remhash key hym-workspace--agent-state)))))

(defun hym-workspace--agent-state-stale-p (entry)
  "Return non-nil when ENTRY should be discarded as stale."
  (and (eq (plist-get entry :state) 'working)
       (let ((updated-at (plist-get entry :updated-at)))
         (and updated-at
              (> (- (float-time) updated-at)
                 hym-workspace-agent-working-timeout)))))

(defun hym-workspace--agent-sweep-stale-working ()
  "Clear stale `working' agent states.
Return non-nil when anything changed."
  (let (keys)
    (maphash
     (lambda (key entry)
       (when (hym-workspace--agent-state-stale-p entry)
         (push key keys)))
     hym-workspace--agent-state)
    (dolist (key keys)
      (remhash key hym-workspace--agent-state))
    keys))

(defun hym-workspace--agent-ensure-stale-timer ()
  "Start the stale-agent timer if it is not already running."
  (unless (timerp hym-workspace--agent-stale-timer)
    (setq hym-workspace--agent-stale-timer
          (run-at-time
           hym-workspace-agent-stale-check-interval
           hym-workspace-agent-stale-check-interval
           (lambda ()
             (when (hym-workspace--agent-sweep-stale-working)
               (hym-workspace-refresh-ui)))))))

(defun hym-workspace--agent-event-state (event old)
  "Return EVENT's new state, using OLD for unknown events."
  (pcase event
    ((or "UserPromptSubmit" "PreToolUse" "PostToolUse" "PostToolUseFailure"
         "SessionStart")
     'working)
    ("Stop" 'waiting)
    ("PermissionRequest" 'permission)
    ("agent_needs_input" 'question)
    ("SessionEnd" nil)
    ;; The generic Notification event is deliberately unhandled: it fires for
    ;; many subtypes (idle_prompt after 60s idle, agent_completed, auth_success),
    ;; not just the actionable ones, so mapping it wholesale to `permission'
    ;; flipped the waiting badge whenever a finished agent sat idle. We instead
    ;; cherry-pick the subtypes worth a badge via per-matcher registrations (see
    ;; install-hooks): permission dialogs arrive as PermissionRequest, and a
    ;; blocked question as `agent_needs_input' above.
    (_ old)))

(defun hym-workspace-agent-signal (slug agent session event)
  "Update SLUG's AGENT/SESSION state from hook EVENT, and refresh.
Only re-renders when the state actually changes, so the stream of tool
events during an active turn doesn't churn the sidebar. Called from the
hook via `emacsclient --eval'."
  (let* ((agent (or agent "agent"))
         (session (or session "default"))
         (key (hym-workspace--agent-state-key slug session))
         (entry (gethash key hym-workspace--agent-state))
         (old (plist-get entry :state))
         (new (hym-workspace--agent-event-state event old)))
    (cond
     ;; An unchanged state still refreshes the timestamp, so a long turn is
     ;; not swept up as stale, but needs no redraw.
     ((and new (eq new old))
      (plist-put entry :updated-at (float-time))
      (puthash key entry hym-workspace--agent-state)
      (hym-workspace--agent-ensure-stale-timer))
     (new
      (puthash key
               (list :slug slug :agent agent :session session
                     :state new :updated-at (float-time))
               hym-workspace--agent-state)
      (hym-workspace--agent-ensure-stale-timer)
      (hym-workspace-refresh-ui))
     (entry
      (hym-workspace--agent-clear-state slug session)
      (hym-workspace-refresh-ui)))))

(defun hym-workspace--agent-entries (slug)
  "Return non-stale tracked agent entries for SLUG."
  (let (entries keys)
    (maphash
     (lambda (key entry)
       (when (equal slug (car-safe key))
         (if (hym-workspace--agent-state-stale-p entry)
             (push key keys)
           (push entry entries))))
     hym-workspace--agent-state)
    (dolist (key keys)
      (remhash key hym-workspace--agent-state))
    (when keys (hym-workspace-refresh-ui))
    (sort entries
          (lambda (a b)
            (string< (format "%s/%s" (plist-get a :agent) (plist-get a :session))
                     (format "%s/%s" (plist-get b :agent) (plist-get b :session)))))))

(defun hym-workspace--agent-duplicate-agent-p (agent entries)
  "Return non-nil when AGENT occurs more than once in ENTRIES."
  (> (cl-count agent entries :key (lambda (entry) (plist-get entry :agent))
               :test #'equal)
     1))

(defun hym-workspace--agent-short-session (session)
  "Return a compact display suffix for SESSION."
  (if (> (length session) 6)
      (substring session -6)
    session))

(defun hym-workspace--agent-badge-line (entry entries)
  "Return the sidebar badge line for agent ENTRY among ENTRIES."
  (let* ((agent (plist-get entry :agent))
         (label (if (hym-workspace--agent-duplicate-agent-p agent entries)
                    (format "%s#%s"
                            agent
                            (hym-workspace--agent-short-session
                             (plist-get entry :session)))
                  agent)))
    (pcase (plist-get entry :state)
      ('working (format "- %s running" label))
      ('waiting (propertize (format "~ %s waiting" label) 'face 'warning))
      ('question (propertize (format "? %s needs input" label) 'face 'warning))
      ('permission (propertize (format "! %s needs permission" label)
                               'face 'error)))))

(defun hym-workspace--agent-badge (ws)
  "Status function: badge lines for WS's tracked agent sessions."
  (let ((entries (hym-workspace--agent-entries (hym-workspace--key ws))))
    (mapcar (lambda (entry)
              (hym-workspace--agent-badge-line entry entries))
            entries)))

(defun hym-workspace--server-badge (ws)
  "Status function: one badge line for each live server process in WS."
  (mapcar (lambda (entry)
            (propertize (format "● %s server running" (car entry))
                        'face 'success))
          (hym-workspace--live-servers (hym-workspace--key ws))))

(defun hym-workspace--server-buffer-live-p (name)
  "Return non-nil when the server buffer NAME still has a live process."
  (let ((buf (and name (get-buffer name))))
    (and buf (process-live-p (get-buffer-process buf)))))

(defun hym-workspace--sweep-servers (predicate)
  "Return (SERVER-KEY . BUFFER-NAME) for live servers matching PREDICATE.
Dead entries are dropped from the table on the way past, whether or not
they match."
  (let (live dead)
    (maphash
     (lambda (server-key name)
       (if (not (hym-workspace--server-buffer-live-p name))
           (push server-key dead)
         (when (funcall predicate server-key)
           (push (cons server-key name) live))))
     hym-workspace--servers)
    (dolist (server-key dead)
      (remhash server-key hym-workspace--servers))
    live))

(defun hym-workspace--live-servers (workspace-key)
  "Return live (REPO . BUFFER-NAME) servers for WORKSPACE-KEY.
Remove dead server entries while collecting the result."
  (sort (mapcar (lambda (entry) (cons (cadr (car entry)) (cdr entry)))
                (hym-workspace--sweep-servers
                 (lambda (server-key)
                   (equal workspace-key (car-safe server-key)))))
        (lambda (a b) (string< (car a) (car b)))))

(with-eval-after-load 'hym-workspaces-sidebar
  (add-to-list 'hym-workspace-sidebar-status-functions #'hym-workspace--server-badge)
  (add-to-list 'hym-workspace-sidebar-status-functions #'hym-workspace--agent-badge))

(defun hym-workspace--teardown-servers (ws)
  "Stop WS's servers, leaving the redraw to whoever is tearing WS down."
  (hym-workspace-kill-workspace-servers ws t))

(add-hook 'hym-workspace-teardown-functions #'hym-workspace--teardown-servers)

(defun hym-workspace--repos-with-run (ws)
  "Return WS's repos whose conductor.json defines a `run' script."
  (let ((code (expand-file-name hym-workspace-code-root)))
    (seq-filter
     (lambda (repo)
       (alist-get 'run (hym-workspace--repo-conductor
                        (expand-file-name repo code))))
     (hym-workspace-repos ws))))

(defun hym-workspace--pick-agent ()
  "Return a chosen (NAME . COMMAND) from `hym-workspace-agents'.
Prompt only when more than one agent is configured."
  (cond ((null hym-workspace-agents)
         (user-error "No agents configured (see `hym-workspace-agents')"))
        ((null (cdr hym-workspace-agents)) (car hym-workspace-agents))
        (t (assoc (completing-read "Agent: " (mapcar #'car hym-workspace-agents) nil t)
                  hym-workspace-agents))))

(defun hym-workspace--agent-session-id (name)
  "Return a fresh session id for an agent NAME."
  (format "%s-%x-%x" name (emacs-pid) (random most-positive-fixnum)))

(defun hym-workspace--agent-env (ws name &optional session)
  "Return the identifying env vars for WS's NAME agent terminal.
Keyed on `hym-workspace--key' so project/notes workspaces (no slug) get a
real key rather than the literal \"nil\"."
  (list (format "HYM_WORKSPACE_SLUG=%s" (hym-workspace--key ws))
        (format "HYM_WORKSPACE_AGENT=%s" name)
        (format "HYM_WORKSPACE_AGENT_SESSION=%s"
                (or session (hym-workspace--agent-session-id name)))))

(defun hym-workspace-run-shell ()
  "Open a shell tab at the current workspace's root."
  (interactive)
  (when-let* ((ws (hym-workspace-current)))
    (hym-workspace-spawn-tab
     ws "shell"
     (lambda ()
       ;; `ghostel' with no arg reuses one global terminal; t forces a fresh
       ;; one so each tab is its own shell.
       (let ((default-directory (hym-workspace-root ws)))
         (ghostel t))))))

(defun hym-workspace--server-live-p (workspace-key repo)
  "Return non-nil when REPO has a live server in WORKSPACE-KEY."
  (hym-workspace--server-buffer-live-p
   (gethash (list workspace-key repo) hym-workspace--servers)))

(defun hym-workspace--rename-server-tab (buf)
  "Mark every tab containing BUF as an old server tab."
  (when (and (buffer-live-p buf) (fboundp 'tab-bar-get-buffer-tab))
    (let ((server-tabs (tab-bar-get-buffer-tab buf nil nil t))
          (tabs (funcall tab-bar-tabs-function))
          renamed)
      (dolist (tab server-tabs)
        (let* ((name (alist-get 'name tab))
               (pos (and name
                         (cl-position name tabs
                                      :key (lambda (candidate)
                                             (alist-get 'name candidate))
                                      :test #'equal))))
          (when (and pos
                     (not (member pos renamed))
                     (stringp name)
                     (not (string-prefix-p hym-workspace-old-server-tab-prefix
                                           name)))
            (push pos renamed)
            (tab-bar-rename-tab
             (concat hym-workspace-old-server-tab-prefix name)
             (1+ pos))))))))

(defun hym-workspace--old-server-buffer-name (name)
  "Return an old-server buffer name derived from NAME."
  (if (string-prefix-p "*" name)
      (concat "*" hym-workspace-old-server-tab-prefix (substring name 1))
    (concat hym-workspace-old-server-tab-prefix name)))

(defun hym-workspace--rename-server-buffer (buf)
  "Mark BUF as an old server buffer."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (unless (string-prefix-p hym-workspace-old-server-tab-prefix
                               (string-remove-prefix "*" (buffer-name)))
        (rename-buffer
         (generate-new-buffer-name
          (hym-workspace--old-server-buffer-name (buffer-name)))
         t)))))

(defun hym-workspace--finish-killing-server (proc buf &optional after-kill)
  "Force-kill PROC if necessary, clean up BUF, then call AFTER-KILL."
  (when (process-live-p proc)
    (ignore-errors (kill-process proc)))
  (when (buffer-live-p buf)
    (kill-buffer buf))
  (when after-kill
    (funcall after-kill)))

(defun hym-workspace--kill-server (workspace-key repo
                                                &optional defer-refresh after-kill)
  "Asynchronously kill REPO's tracked server in WORKSPACE-KEY.
Send SIGTERM first, then SIGKILL after
`hym-workspace-server-shutdown-timeout' seconds if it is still alive.
The retired buffer is cleaned up after that timeout.
When DEFER-REFRESH is non-nil, leave sidebar refresh to the caller.
Call AFTER-KILL only once the old process has been killed and cleaned up."
  (let* ((server-key (list workspace-key repo))
         (name (gethash server-key hym-workspace--servers))
         (buf (and name (get-buffer name)))
         (proc (and buf (get-buffer-process buf))))
    (if (process-live-p proc)
        (progn
          (hym-workspace--rename-server-tab buf)
          (hym-workspace--rename-server-buffer buf)
          (set-process-query-on-exit-flag proc nil)
          ;; Ghostel's normal process filter renders every shutdown message,
          ;; and its compile sentinel synchronously redraws and parses the
          ;; whole buffer on exit.  Detach both before signalling so a noisy
          ;; or large server shutdown cannot monopolize Emacs.
          (set-process-filter proc #'ignore)
          (set-process-sentinel proc #'ignore)
          (set-process-buffer proc nil)
          (ignore-errors (signal-process proc 'SIGTERM))
          (when (buffer-live-p buf)
            (kill-buffer buf))
          (run-at-time hym-workspace-server-shutdown-timeout nil
                       #'hym-workspace--finish-killing-server
                       proc buf after-kill))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when after-kill
        (funcall after-kill)))
    (remhash server-key hym-workspace--servers)
    (unless defer-refresh
      (hym-workspace-refresh-ui))))

(defun hym-workspace-kill-workspace-servers (ws &optional defer-refresh)
  "Kill every tracked server for WS.
When DEFER-REFRESH is non-nil, leave sidebar refresh to the caller."
  (let* ((key (hym-workspace--key ws))
         (repos (mapcar #'car (hym-workspace--live-servers key))))
    (dolist (repo repos)
      (hym-workspace--kill-server key repo t))
    (unless defer-refresh
      (hym-workspace-refresh-ui))
    repos))

(defun hym-workspace--running-server-choices ()
  "Return (DISPLAY . SERVER-KEY) choices for every live tracked server."
  (sort (mapcar (lambda (entry)
                  (let ((server-key (car entry)))
                    (cons (format "%s/%s" (car server-key) (cadr server-key))
                          server-key)))
                (hym-workspace--sweep-servers #'always))
        (lambda (a b) (string< (car a) (car b)))))

(defun hym-workspace-kill-server ()
  "Prompt for and kill any running workspace server."
  (interactive)
  (let ((choices (hym-workspace--running-server-choices)))
    (unless choices
      (user-error "No workspace servers are running"))
    (let* ((selected (completing-read "Kill server: " choices nil t))
           (server-key (cdr (assoc selected choices))))
      (hym-workspace--kill-server (car server-key) (cadr server-key))
      (message "Killed %s" selected))))

(defun hym-workspace--start-server (ws repo)
  "Start REPO's conductor `run' script in a server tab for WS."
  (let* ((key (hym-workspace--key ws))
         (code (expand-file-name hym-workspace-code-root))
         (run (alist-get 'run (hym-workspace--repo-conductor
                               (expand-file-name repo code))))
         (bufname (format "*ws-server: %s/%s*" key repo)))
    (hym-workspace-spawn-tab
     ws (format "server:%s" repo)
     (lambda ()
       (let ((default-directory (expand-file-name repo (hym-workspace-root ws)))
             (ghostel-compile-buffer-name bufname)
             (ghostel-environment
              (append (hym-workspace--server-environment repo)
                      (hym-workspace--ghostel-environment))))
         (ghostel-compile run t))
       ;; ghostel-compile splits; make its output fill the new tab's main
       ;; window instead of sitting beside the cloned old buffer. The
       ;; sidebar survives via its no-delete-other-windows parameter.
       (when-let* ((buf (get-buffer bufname)))
         (switch-to-buffer buf)
         (delete-other-windows))
       (puthash (list key repo) bufname hym-workspace--servers)
       (when-let* ((proc (get-buffer-process (get-buffer bufname))))
         (add-function :after (process-sentinel proc)
                       (lambda (&rest _) (hym-workspace-refresh-ui))))
       (hym-workspace-refresh-ui)))))

(defun hym-workspace-run-server ()
  "Run a repo's conductor `run' script in a server tab, with live output."
  (interactive)
  (when-let* ((ws (hym-workspace-current)))
    (let ((key (hym-workspace--key ws)))
      (let* ((repos (hym-workspace--repos-with-run ws))
             (repo (cond ((null repos) (user-error "No repo has a run script"))
                         ((null (cdr repos)) (car repos))
                         (t (completing-read "Server repo: " repos nil t)))))
        (if (hym-workspace--server-live-p key repo)
            (if (yes-or-no-p
                 (format "The %s server is already running; kill and restart it? "
                         repo))
                (hym-workspace--kill-server
                 key repo nil
                 (lambda () (hym-workspace--start-server ws repo)))
              (user-error "The %s server is still running" repo))
          (hym-workspace--start-server ws repo))))))

(defun hym-workspace-run-all-servers ()
  "Run every configured server in the current workspace.
Servers that are already live are left running."
  (interactive)
  (when-let* ((ws (hym-workspace-current)))
    (let* ((key (hym-workspace--key ws))
           (repos (hym-workspace--repos-with-run ws))
           started)
      (unless repos
        (user-error "No repo has a run script"))
      (dolist (repo repos)
        (unless (hym-workspace--server-live-p key repo)
          (hym-workspace--start-server ws repo)
          (push repo started)))
      (if started
          (message "Started %d server%s" (length started)
                   (if (= (length started) 1) "" "s"))
        (message "All workspace servers are already running")))))

(defun hym-workspace-restart-running-servers ()
  "Restart every live server in the current workspace.
Servers that are not currently running are left stopped."
  (interactive)
  (when-let* ((ws (hym-workspace-current)))
    (let* ((key (hym-workspace--key ws))
           (repos (mapcar #'car (hym-workspace--live-servers key))))
      (unless repos
        (user-error "No servers are running in this workspace"))
      (cl-loop for repo in repos
               for delay from 0 by hym-workspace-restart-server-delay
               do (let ((repo repo)
                        (delay delay))
                    (hym-workspace--kill-server
                     key repo t
                     (lambda ()
                       (run-at-time delay nil
                                    #'hym-workspace--start-server ws repo)))))
      (hym-workspace-refresh-ui)
      (message "Restarting %d server%s" (length repos)
               (if (= (length repos) 1) "" "s")))))

(defun hym-workspace--shell-quote (s)
  "Single-quote S for the login shell (fish), which the agent terminal runs.
Wrapping in single quotes with each embedded quote rendered as '\\'' is
literal in both fish and POSIX sh; `shell-quote-argument's backslash form
is unsafe against fish globbing/history expansion."
  (concat "'" (replace-regexp-in-string "'" "'\\''" s nil t) "'"))

(defun hym-workspace--agent-launch-string (command prompt)
  "Return the shell line launching COMMAND, seeded with PROMPT when non-blank."
  (if (and prompt (not (string-empty-p (string-trim prompt))))
      (format "%s %s" command (hym-workspace--shell-quote prompt))
    command))

(defun hym-workspace--start-agent (ws name command &optional session prompt)
  "Open an agent tab in WS at its root and start NAME's COMMAND.
Reuse SESSION when given, else mint one. Seed PROMPT as a shell-quoted
argument when non-blank."
  (let ((key (hym-workspace--key ws))
        (session (or session (hym-workspace--agent-session-id name))))
    (hym-workspace-spawn-tab
     ws "agent"
     (lambda ()
       ;; t forces a fresh terminal so the process actually spawns and the
       ;; injected env (HYM_WORKSPACE_SLUG) takes effect.
       (let ((default-directory (hym-workspace-root ws))
             (ghostel-environment
              (append (hym-workspace--agent-env ws name session)
                      (hym-workspace--ghostel-environment))))
         (ghostel t))
       ;; Clear agent state if the terminal dies without a clean SessionEnd,
       ;; so a stale waiting/permission badge doesn't stick.
       (add-hook 'kill-buffer-hook
                 (lambda ()
                   (hym-workspace--agent-clear-state key session)
                   (hym-workspace-refresh-ui))
                 nil t)
       (ghostel-send-string
        (concat (hym-workspace--agent-launch-string command prompt) "\n"))))))

(defun hym-workspace--preset-agent-command (preset)
  "Return (NAME . COMMAND) for PRESET's agent, defaulting to the first agent."
  (let ((name (hym-workspace-preset-agent preset)))
    (or (and name (assoc name hym-workspace-agents))
        (car hym-workspace-agents)
        (user-error "No agents configured (see `hym-workspace-agents')"))))

(defun hym-workspace-new-from-preset (preset prompt)
  "Create a worktree workspace from PRESET and start its agent seeded with PROMPT."
  (interactive
   (let ((presets (hym-workspace-presets)))
     (unless presets
       (user-error "No presets defined; create %s" hym-workspace-presets-file))
     (let* ((name (completing-read
                   "Preset: " (mapcar #'hym-workspace-preset-name presets) nil t))
            (preset (seq-find
                     (lambda (p) (equal (hym-workspace-preset-name p) name))
                     presets)))
       (list preset (read-string "Prompt: ")))))
  (let* ((agent (hym-workspace--preset-agent-command preset))
         (name (hym-workspace--name-from-prompt prompt))
         (repos (hym-workspace-preset-repos preset))
         (ws (hym-workspace--register-worktree
              name (hym-workspace-preset-base-branch preset) repos)))
    (make-directory (hym-workspace-root ws) t)
    (hym-workspace-open ws)
    (hym-workspace--provision
     ws repos nil
     (lambda (ok)
       (if ok
           (hym-workspace--start-agent ws (car agent) (cdr agent) nil prompt)
         (hym-workspace--show-setup-error ws))))))

(defun hym-workspace-run-agent ()
  "Open an agent tab at the workspace root and start the chosen agent."
  (interactive)
  (when-let* ((ws (hym-workspace-current)))
    (let ((agent (hym-workspace--pick-agent)))
      (hym-workspace--start-agent ws (car agent) (cdr agent)))))

(defun hym-workspace-run-agent-shell ()
  "Open an `agent-shell' tab at the workspace root."
  (interactive)
  (unless (or (fboundp 'agent-shell-new-shell)
              (require 'agent-shell nil t))
    (user-error "agent-shell is not available"))
  (when-let* ((ws (hym-workspace-current)))
    (let* ((key (hym-workspace--key ws))
           (session (hym-workspace--agent-session-id "agent-shell")))
      (hym-workspace-spawn-tab
       ws "agent-shell"
       (lambda ()
         (let ((default-directory (hym-workspace-root ws))
               (process-environment
                (append (hym-workspace--agent-env ws "agent-shell" session)
                        process-environment)))
           (agent-shell-new-shell))
         ;; Clear agent state if the shell buffer dies without a clean
         ;; SessionEnd, matching the Ghostty agent tab behaviour.
         (add-hook 'kill-buffer-hook
                   (lambda ()
                     (hym-workspace--agent-clear-state key session)
                     (hym-workspace-refresh-ui))
                   nil t))))))

(provide 'hym-workspaces-run)

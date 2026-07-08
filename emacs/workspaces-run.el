;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'seq)

;; Declared special so our `let' binds them dynamically even before
;; ghostel-compile.el (autoloaded) defines `ghostel-compile-buffer-name';
;; without this the lexical-binding file binds it lexically and the later
;; defcustom errors with "Defining as dynamic an already lexical var".
(defvar ghostel-compile-buffer-name)
(defvar ghostel-environment)
(defvar agent-shell-buffer-name)
(declare-function agent-shell-new-shell "agent-shell")

(defcustom hym-workspace-agents '(("claude" . "claude") ("codex" . "codex"))
  "Alist of (NAME . SHELL-COMMAND) for agents launchable in a workspace."
  :type '(alist :key-type string :value-type string) :group 'hym-workspace)

(defvar hym-workspace--servers (make-hash-table :test 'equal)
  "Map workspace slug to the name of its server (`ghostel-compile') buffer.")

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

(defun hym-workspace--run-refresh ()
  (when (fboundp 'hym-workspace-sidebar-refresh)
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace--agent-state-key (slug session)
  "Return the hash key for SLUG and SESSION."
  (list slug session))

(defun hym-workspace--agent-entry-slug (key entry)
  "Return the workspace slug represented by KEY and ENTRY."
  (or (plist-get (and (consp entry) entry) :slug)
      (and (stringp key) key)))

(defun hym-workspace--agent-entry-state (entry)
  "Return the state represented by ENTRY."
  (if (consp entry)
      (plist-get entry :state)
    entry))

(defun hym-workspace--agent-entry-updated-at (entry)
  "Return ENTRY's last update time, or nil for legacy entries."
  (when (consp entry)
    (plist-get entry :updated-at)))

(defun hym-workspace--agent-normalize-entry (key entry)
  "Return ENTRY as a session plist, accepting the old slug -> state shape."
  (if (consp entry)
      entry
    (list :slug key :agent "agent" :session "default"
          :state entry :updated-at nil)))

(defun hym-workspace--agent-clear-state (slug &optional session)
  "Clear tracked agent state for SLUG.
When SESSION is non-nil, clear only that session."
  (if session
      (progn
        (remhash (hym-workspace--agent-state-key slug session)
                 hym-workspace--agent-state)
        (when (equal session "default")
          (remhash slug hym-workspace--agent-state)))
    (let (keys)
      (maphash
       (lambda (key entry)
         (when (equal slug (hym-workspace--agent-entry-slug key entry))
           (push key keys)))
       hym-workspace--agent-state)
      (dolist (key keys)
        (remhash key hym-workspace--agent-state)))))

(defun hym-workspace--agent-state-stale-p (entry)
  "Return non-nil when ENTRY should be discarded as stale."
  (and (eq (hym-workspace--agent-entry-state entry) 'working)
       (let ((updated-at (hym-workspace--agent-entry-updated-at entry)))
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
               (hym-workspace--run-refresh)))))))

(defun hym-workspace--agent-event-state (event old)
  "Return EVENT's new state, using OLD for unknown events."
  (pcase event
    ((or "UserPromptSubmit" "PreToolUse" "PostToolUse" "SessionStart")
     'working)
    ("Stop" 'waiting)
    ((or "Notification" "PermissionRequest") 'permission)
    ("SessionEnd" nil)
    (_ old)))

(defun hym-workspace-agent-signal (slug &rest args)
  "Update SLUG's agent state from hook ARGS, and refresh.
The preferred call shape is (SLUG AGENT SESSION EVENT).  The older
(SLUG EVENT) shape is accepted as a compatibility fallback.
Only re-renders when the state actually changes, so the stream of tool
events during an active turn doesn't churn the sidebar. Called from the
hook via `emacsclient --eval'."
  (let* ((agent (if (= (length args) 1) "agent" (or (nth 0 args) "agent")))
         (session (if (= (length args) 1) "default" (or (nth 1 args) "default")))
         (event (if (= (length args) 1) (car args) (nth 2 args)))
         (key (hym-workspace--agent-state-key slug session))
         (entry (gethash key hym-workspace--agent-state))
         (legacy-entry (and (equal session "default")
                            (gethash slug hym-workspace--agent-state)))
         (old (plist-get entry :state))
         (new (hym-workspace--agent-event-state event old)))
    (if (and (null new) (equal event "SessionEnd") (or entry legacy-entry))
        (progn
          (hym-workspace--agent-clear-state slug session)
          (hym-workspace--run-refresh))
      (if (eq new old)
        (when new
          (plist-put entry :updated-at (float-time))
          (puthash key entry hym-workspace--agent-state)
          (hym-workspace--agent-ensure-stale-timer))
        (if new
            (progn
              (puthash key
                       (list :slug slug :agent agent :session session
                             :state new :updated-at (float-time))
                       hym-workspace--agent-state)
              (hym-workspace--agent-ensure-stale-timer))
          (hym-workspace--agent-clear-state slug session))
        (hym-workspace--run-refresh)))))

(defun hym-workspace--agent-entries (slug)
  "Return non-stale tracked agent entries for SLUG."
  (let (entries keys)
    (maphash
     (lambda (key entry)
       (when (equal slug (hym-workspace--agent-entry-slug key entry))
         (if (hym-workspace--agent-state-stale-p entry)
             (push key keys)
           (push (hym-workspace--agent-normalize-entry key entry)
                 entries))))
     hym-workspace--agent-state)
    (dolist (key keys)
      (remhash key hym-workspace--agent-state))
    (when keys (hym-workspace--run-refresh))
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
      ('permission (propertize (format "! %s needs permission" label)
                               'face 'error)))))

(defun hym-workspace--agent-badge (ws)
  "Status function: badge lines for WS's tracked agent sessions."
  (let ((entries (hym-workspace--agent-entries (hym-workspace--key ws))))
    (mapcar (lambda (entry)
              (hym-workspace--agent-badge-line entry entries))
            entries)))

(defun hym-workspace--server-badge (ws)
  "Status function: a badge line while WS's server process is live."
  (let* ((key (hym-workspace--key ws))
         (name (gethash key hym-workspace--servers))
         (buf (and name (get-buffer name))))
    (if (and buf (process-live-p (get-buffer-process buf)))
        (list (propertize "● server running" 'face 'success))
      (remhash key hym-workspace--servers)
      nil)))

(with-eval-after-load 'hym-workspaces-sidebar
  (add-to-list 'hym-workspace-sidebar-status-functions #'hym-workspace--server-badge)
  (add-to-list 'hym-workspace-sidebar-status-functions #'hym-workspace--agent-badge))

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
  (when-let ((ws (hym-workspace-current)))
    (hym-workspace-spawn-tab
     ws "shell"
     (lambda ()
       ;; `ghostel' with no arg reuses one global terminal; t forces a fresh
       ;; one so each tab is its own shell.
       (let ((default-directory (hym-workspace-root ws)))
         (ghostel t))))))

(defun hym-workspace--server-live-p (slug)
  "Return non-nil when SLUG has a live tracked server process."
  (let* ((name (gethash slug hym-workspace--servers))
         (buf (and name (get-buffer name))))
    (and buf (process-live-p (get-buffer-process buf)))))

(defun hym-workspace-run-server ()
  "Run a repo's conductor `run' script in a server tab, with live output."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (let ((key (hym-workspace--key ws)))
      (when (hym-workspace--server-live-p key)
        (user-error "A server is already running for this workspace"))
      (let* ((repos (hym-workspace--repos-with-run ws))
             (repo (cond ((null repos) (user-error "No repo has a run script"))
                         ((null (cdr repos)) (car repos))
                         (t (completing-read "Server repo: " repos nil t))))
             (code (expand-file-name hym-workspace-code-root))
             (run (alist-get 'run (hym-workspace--repo-conductor
                                   (expand-file-name repo code))))
             (bufname (format "*ws-server: %s/%s*" key repo)))
        (hym-workspace-spawn-tab
         ws (format "server:%s" repo)
         (lambda ()
           (let ((default-directory (expand-file-name repo (hym-workspace-root ws)))
                 (ghostel-compile-buffer-name bufname))
             (ghostel-compile run t))
           ;; ghostel-compile splits; make its output fill the new tab's main
           ;; window instead of sitting beside the cloned old buffer. The
           ;; sidebar survives via its no-delete-other-windows parameter.
           (when-let ((buf (get-buffer bufname)))
             (switch-to-buffer buf)
             (delete-other-windows))
           (puthash key bufname hym-workspace--servers)
           (when-let ((proc (get-buffer-process (get-buffer bufname))))
             (add-function :after (process-sentinel proc)
                           (lambda (&rest _) (hym-workspace--run-refresh))))
           (hym-workspace--run-refresh)))))))

(defun hym-workspace-run-agent ()
  "Open an agent tab at the workspace root and start the chosen agent."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (let* ((agent (hym-workspace--pick-agent))
           (name (car agent))
           (command (cdr agent))
           (key (hym-workspace--key ws))
           (session (hym-workspace--agent-session-id name)))
      (hym-workspace-spawn-tab
       ws "agent"
       (lambda ()
         ;; t forces a fresh terminal so the process actually spawns and the
         ;; injected env (HYM_WORKSPACE_SLUG) takes effect.
         (let ((default-directory (hym-workspace-root ws))
               (ghostel-environment
                (append (hym-workspace--agent-env ws name session)
                        ghostel-environment)))
           (ghostel t))
         ;; Clear agent state if the terminal dies without a clean SessionEnd,
         ;; so a stale waiting/permission badge doesn't stick.
         (add-hook 'kill-buffer-hook
                   (lambda ()
                     (hym-workspace--agent-clear-state key session)
                     (hym-workspace--run-refresh))
                   nil t)
         (ghostel-send-string (concat command "\n")))))))

(defun hym-workspace-run-agent-shell ()
  "Open an `agent-shell' tab at the workspace root."
  (interactive)
  (unless (or (fboundp 'agent-shell-new-shell)
              (require 'agent-shell nil t))
    (user-error "agent-shell is not available"))
  (when-let ((ws (hym-workspace-current)))
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
                     (hym-workspace--run-refresh))
                   nil t))))))

(provide 'hym-workspaces-run)

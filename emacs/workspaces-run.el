;; -*- lexical-binding: t -*-
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
  "Map workspace slug to agent state: working | waiting | permission.")

(defun hym-workspace--run-refresh ()
  (when (fboundp 'hym-workspace-sidebar-refresh)
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace-agent-signal (slug event)
  "Update SLUG's agent state from a Claude Code hook EVENT, and refresh.
Only re-renders when the state actually changes, so the stream of tool
events during an active turn doesn't churn the sidebar. Called from the
hook via `emacsclient --eval'."
  (let ((old (gethash slug hym-workspace--agent-state))
        (new (pcase event
               ((or "UserPromptSubmit" "PreToolUse" "PostToolUse" "SessionStart")
                'working)
               ("Stop" 'waiting)
               ((or "Notification" "PermissionRequest") 'permission)
               ("SessionEnd" nil)
               (_ (gethash slug hym-workspace--agent-state)))))
    (unless (eq new old)
      (if new
          (puthash slug new hym-workspace--agent-state)
        (remhash slug hym-workspace--agent-state))
      (hym-workspace--run-refresh))))

(defun hym-workspace--agent-badge (ws)
  "Status function: a badge line when WS's agent wants attention."
  (pcase (gethash (hym-workspace--key ws) hym-workspace--agent-state)
    ('waiting (list (propertize "⏳ agent waiting" 'face 'warning)))
    ('permission (list (propertize "⛔ needs permission" 'face 'error)))))

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

(defun hym-workspace--agent-env (ws name)
  "Return the identifying env vars for WS's NAME agent terminal.
Keyed on `hym-workspace--key' so project/notes workspaces (no slug) get a
real key rather than the literal \"nil\"."
  (list (format "HYM_WORKSPACE_SLUG=%s" (hym-workspace--key ws))
        (format "HYM_WORKSPACE_AGENT=%s" name)))

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
           (key (hym-workspace--key ws)))
      (hym-workspace-spawn-tab
       ws "agent"
       (lambda ()
         ;; t forces a fresh terminal so the process actually spawns and the
         ;; injected env (HYM_WORKSPACE_SLUG) takes effect.
         (let ((default-directory (hym-workspace-root ws))
               (ghostel-environment
                (append (hym-workspace--agent-env ws name) ghostel-environment)))
           (ghostel t))
         ;; Clear agent state if the terminal dies without a clean SessionEnd,
         ;; so a stale waiting/permission badge doesn't stick.
         (add-hook 'kill-buffer-hook
                   (lambda ()
                     (remhash key hym-workspace--agent-state)
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
    (let ((key (hym-workspace--key ws)))
      (hym-workspace-spawn-tab
       ws "agent-shell"
       (lambda ()
         (let ((default-directory (hym-workspace-root ws))
               (process-environment
                (append (hym-workspace--agent-env ws "agent-shell")
                        process-environment)))
           (agent-shell-new-shell))
         ;; Clear agent state if the shell buffer dies without a clean
         ;; SessionEnd, matching the Ghostty agent tab behaviour.
         (add-hook 'kill-buffer-hook
                   (lambda ()
                     (remhash key hym-workspace--agent-state)
                     (hym-workspace--run-refresh))
                   nil t))))))

(provide 'hym-workspaces-run)

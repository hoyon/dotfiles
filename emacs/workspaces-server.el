;;; workspaces-server.el --- Per-workspace dev servers  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Runs a repo's conductor `run' script as a workspace server and tracks it
;; for the sidebar badge, restarts and teardown.
;;
;; A server is a `hym/workspace-server' record.  How and where it runs
;; belongs to its runner: a struct type that :includes the record and
;; implements the `hym/workspace-server-*' generics.
;; `hym/workspace--make-server' picks the runner for a workspace;
;; everything else here only goes through the generics.
;;
;; Worktrees of one repo share its dev ports, so a host runs at most one
;; server per repo: starting one stops the same repo's server in any other
;; workspace on that host.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;; Declared special so our `let' binds them dynamically even before
;; ghostel-compile.el (autoloaded) defines `ghostel-compile-buffer-name';
;; without this the lexical-binding file binds it lexically and the later
;; defcustom errors with "Defining as dynamic an already lexical var".
(defvar ghostel-compile-buffer-name)
(defvar ghostel-environment)
(declare-function hym/workspace--ghostel-environment "workspaces-run")

(defcustom hym/workspace-server-environments-file
  (expand-file-name "server-environments.eld" hym/workspace-home)
  "File mapping repo names to environment variables for their servers.
The file contains an alist whose keys are repo names and whose values are
lists of \"KEY=VALUE\" strings.  It is read on every server start, so edits
take effect without restarting Emacs."
  :type 'file :group 'hym/workspace)

(defcustom hym/workspace-restart-server-delay 0.05
  "Seconds to wait between starting servers during a bulk restart."
  :type 'number :group 'hym/workspace)

(defcustom hym/workspace-server-shutdown-timeout 3
  "Seconds to let a server exit after SIGTERM before sending SIGKILL."
  :type 'number :group 'hym/workspace)

;;;; Server records and runners

(cl-defstruct (hym/workspace-server (:constructor nil) (:copier nil))
  "A workspace server.  WORKSPACE is the workspace key, REPO the repo name."
  workspace repo)

(cl-defgeneric hym/workspace-server-host (server)
  "Return the host SERVER runs on, or nil for this machine.")

(cl-defgeneric hym/workspace-server-live-p (server)
  "Return non-nil while SERVER is running.")

(cl-defgeneric hym/workspace-server-launch (server dir command env)
  "Start SERVER running COMMAND in DIR, showing it in the selected tab.
ENV is a list of extra \"KEY=VALUE\" strings.  Call
`hym/workspace-refresh-ui' when the server exits.")

(cl-defgeneric hym/workspace-server-stop (server after-stop)
  "Stop SERVER, calling AFTER-STOP (when non-nil) once it has gone.")

(cl-defstruct (hym/workspace-local-server
               (:include hym/workspace-server)
               (:constructor hym/workspace-local-server-create)
               (:copier nil))
  "A server running in a `ghostel-compile' buffer named BUFFER."
  buffer)

(cl-defmethod hym/workspace-server-host ((_server hym/workspace-local-server))
  nil)

(defun hym/workspace--local-server-process (server)
  "Return SERVER's buffer and process as (BUFFER . PROCESS), either nil."
  (let ((buf (get-buffer (hym/workspace-local-server-buffer server))))
    (cons buf (and buf (get-buffer-process buf)))))

(cl-defmethod hym/workspace-server-live-p ((server hym/workspace-local-server))
  (process-live-p (cdr (hym/workspace--local-server-process server))))

(cl-defmethod hym/workspace-server-launch ((server hym/workspace-local-server)
                                           dir command env)
  (let ((bufname (hym/workspace-local-server-buffer server)))
    (let ((default-directory dir)
          (ghostel-compile-buffer-name bufname)
          (ghostel-environment
           (append env (hym/workspace--ghostel-environment))))
      (ghostel-compile command t))
    ;; ghostel-compile splits; make its output fill the new tab's main
    ;; window instead of sitting beside the cloned old buffer. The
    ;; sidebar survives via its no-delete-other-windows parameter.
    (when-let* ((buf (get-buffer bufname)))
      (switch-to-buffer buf)
      (delete-other-windows)
      (when-let* ((proc (get-buffer-process buf)))
        (add-function :after (process-sentinel proc)
                      (lambda (&rest _) (hym/workspace-refresh-ui)))))))

(defun hym/workspace--finish-killing-server (proc buf &optional after-kill)
  "Force-kill PROC if necessary, clean up BUF, then call AFTER-KILL."
  (when (process-live-p proc)
    (ignore-errors (kill-process proc)))
  (when (buffer-live-p buf)
    (kill-buffer buf))
  (when after-kill
    (funcall after-kill)))

(cl-defmethod hym/workspace-server-stop ((server hym/workspace-local-server)
                                         after-stop)
  "Send SIGTERM, then SIGKILL after `hym/workspace-server-shutdown-timeout'.
The retired buffer is cleaned up after that timeout."
  (pcase-let ((`(,buf . ,proc) (hym/workspace--local-server-process server)))
    (if (process-live-p proc)
        (progn
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
          (run-at-time hym/workspace-server-shutdown-timeout nil
                       #'hym/workspace--finish-killing-server
                       proc buf after-stop))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (when after-stop
        (funcall after-stop)))))

(defun hym/workspace--make-server (ws repo)
  "Return an unstarted server record for REPO in WS."
  (let ((key (hym/workspace--key ws)))
    (hym/workspace-local-server-create
     :workspace key :repo repo
     :buffer (format "*ws-server: %s/%s*" key repo))))

;;;; Tracking

(defvar hym/workspace--servers (make-hash-table :test 'equal)
  "Map (WORKSPACE-KEY REPO) to its `hym/workspace-server'.")

(defun hym/workspace--server-id (server)
  "Return SERVER's key in `hym/workspace--servers'."
  (list (hym/workspace-server-workspace server)
        (hym/workspace-server-repo server)))

(defun hym/workspace--server-label (server)
  "Return SERVER as \"WORKSPACE-KEY/REPO\"."
  (format "%s/%s"
          (hym/workspace-server-workspace server)
          (hym/workspace-server-repo server)))

(defun hym/workspace--server (workspace-key repo)
  "Return the tracked server for REPO in WORKSPACE-KEY, live or not."
  (gethash (list workspace-key repo) hym/workspace--servers))

(defun hym/workspace--server-live-p (workspace-key repo)
  "Return non-nil when REPO has a live server in WORKSPACE-KEY."
  (when-let* ((server (hym/workspace--server workspace-key repo)))
    (hym/workspace-server-live-p server)))

(defun hym/workspace--sweep-servers (predicate)
  "Return live servers matching PREDICATE, sorted by workspace then repo.
Dead servers are dropped from the table on the way past, whether or not
they match."
  (let (live dead)
    (maphash
     (lambda (id server)
       (cond ((not (hym/workspace-server-live-p server)) (push id dead))
             ((funcall predicate server) (push server live))))
     hym/workspace--servers)
    (dolist (id dead)
      (remhash id hym/workspace--servers))
    (sort live (lambda (a b)
                 (string< (hym/workspace--server-label a)
                          (hym/workspace--server-label b))))))

(defun hym/workspace--live-servers (workspace-key)
  "Return WORKSPACE-KEY's live servers, sorted by repo."
  (hym/workspace--sweep-servers
   (lambda (server)
     (equal workspace-key (hym/workspace-server-workspace server)))))

(defun hym/workspace--conflicting-servers (server)
  "Return live servers competing with SERVER for its repo's dev ports.
That is the same repo on the same host, in another workspace."
  (hym/workspace--sweep-servers
   (lambda (other)
     (and (equal (hym/workspace-server-repo other)
                 (hym/workspace-server-repo server))
          (equal (hym/workspace-server-host other)
                 (hym/workspace-server-host server))
          (not (equal (hym/workspace-server-workspace other)
                      (hym/workspace-server-workspace server)))))))

(defun hym/workspace--server-badge (ws)
  "Status function: one badge line for each live server in WS."
  (mapcar (lambda (server)
            (propertize (format "● %s running" (hym/workspace-server-repo server))
                        'face 'success))
          (hym/workspace--live-servers (hym/workspace--key ws))))

(with-eval-after-load 'hym/workspaces-sidebar
  (add-to-list 'hym/workspace-sidebar-status-functions #'hym/workspace--server-badge))

;;;; Starting and stopping

(defun hym/workspace--server-environment (repo)
  "Return the configured server environment for REPO.
Read `hym/workspace-server-environments-file' on every call.  A missing file
or an unconfigured repo yields nil; malformed entries signal an error rather
than starting a server with an unexpected environment."
  (let ((entries
         (hym/workspace--read-eld hym/workspace-server-environments-file
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
             hym/workspace-server-environments-file))
    (alist-get repo entries nil nil #'equal)))

(defun hym/workspace--repo-run-script (repo)
  "Return REPO's conductor `run' script, or nil."
  (alist-get 'run (hym/workspace--repo-conductor
                   (hym/workspace--code-dir repo))))

(defun hym/workspace--repos-with-run (ws)
  "Return WS's repos whose conductor.json defines a `run' script."
  (seq-filter #'hym/workspace--repo-run-script (hym/workspace-repos ws)))

(defun hym/workspace--stop-server (server &optional defer-refresh after-stop)
  "Stop SERVER and stop tracking it; call AFTER-STOP once it has gone.
When DEFER-REFRESH is non-nil, leave the redraw to the caller."
  ;; Untrack first: AFTER-STOP may synchronously start a replacement under
  ;; the same id.
  (remhash (hym/workspace--server-id server) hym/workspace--servers)
  (hym/workspace-server-stop server after-stop)
  (unless defer-refresh
    (hym/workspace-refresh-ui)))

(defun hym/workspace--stop-servers-then (servers then)
  "Stop every server in SERVERS, then call THEN once."
  (if (null servers)
      (funcall then)
    (let ((remaining (length servers)))
      (dolist (server servers)
        (hym/workspace--stop-server
         server t
         (lambda ()
           (setq remaining (1- remaining))
           (when (zerop remaining)
             (funcall then))))))))

(defun hym/workspace--spawn-server (ws server)
  "Launch SERVER in WS's server tab for its repo, reusing any existing one."
  (let ((repo (hym/workspace-server-repo server)))
    (hym/workspace-spawn-tab
     ws (format "server:%s" repo)
     (lambda ()
       (hym/workspace-server-launch
        server
        (expand-file-name repo (hym/workspace-root ws))
        (hym/workspace--repo-run-script repo)
        (hym/workspace--server-environment repo))
       (puthash (hym/workspace--server-id server) server hym/workspace--servers)
       (hym/workspace-refresh-ui))
     t)))

(defun hym/workspace--start-server (ws repo)
  "Start REPO's server in WS, first stopping any server it conflicts with."
  (let* ((server (hym/workspace--make-server ws repo))
         (conflicts (hym/workspace--conflicting-servers server)))
    (dolist (other conflicts)
      (message "Stopping the %s server in %s"
               repo (hym/workspace-server-workspace other)))
    (hym/workspace--stop-servers-then
     conflicts (lambda () (hym/workspace--spawn-server ws server)))))

(defun hym/workspace-kill-workspace-servers (ws &optional defer-refresh)
  "Kill every live server in WS and return their repos.
When DEFER-REFRESH is non-nil, leave sidebar refresh to the caller."
  (let ((servers (hym/workspace--live-servers (hym/workspace--key ws))))
    (dolist (server servers)
      (hym/workspace--stop-server server t))
    (unless defer-refresh
      (hym/workspace-refresh-ui))
    (mapcar #'hym/workspace-server-repo servers)))

(defun hym/workspace--teardown-servers (ws)
  "Stop WS's servers, leaving the redraw to whoever is tearing WS down."
  (hym/workspace-kill-workspace-servers ws t))

(add-hook 'hym/workspace-teardown-functions #'hym/workspace--teardown-servers)

(defun hym/workspace--running-server-choices ()
  "Return (LABEL . SERVER) choices for every live server."
  (mapcar (lambda (server) (cons (hym/workspace--server-label server) server))
          (hym/workspace--sweep-servers #'always)))

;;;; Commands

(defun hym/workspace-kill-server ()
  "Prompt for and kill any running workspace server."
  (interactive)
  (let ((choices (hym/workspace--running-server-choices)))
    (unless choices
      (user-error "No workspace servers are running"))
    (let ((selected (completing-read "Kill server: " choices nil t)))
      (hym/workspace--stop-server (cdr (assoc selected choices)))
      (message "Killed %s" selected))))

(defun hym/workspace-run-server ()
  "Run a repo's conductor `run' script in a server tab, with live output."
  (interactive)
  (when-let* ((ws (hym/workspace-current)))
    (let* ((key (hym/workspace--key ws))
           (repos (hym/workspace--repos-with-run ws))
           (repo (cond ((null repos) (user-error "No repo has a run script"))
                       ((null (cdr repos)) (car repos))
                       (t (completing-read "Server repo: " repos nil t)))))
      (if (hym/workspace--server-live-p key repo)
          (if (yes-or-no-p
               (format "The %s server is already running; kill and restart it? "
                       repo))
              (hym/workspace--stop-server
               (hym/workspace--server key repo) nil
               (lambda () (hym/workspace--start-server ws repo)))
            (user-error "The %s server is still running" repo))
        (hym/workspace--start-server ws repo)))))

(defun hym/workspace-run-all-servers ()
  "Run every configured server in the current workspace.
Servers that are already live are left running."
  (interactive)
  (when-let* ((ws (hym/workspace-current)))
    (let* ((key (hym/workspace--key ws))
           (repos (hym/workspace--repos-with-run ws))
           started)
      (unless repos
        (user-error "No repo has a run script"))
      (dolist (repo repos)
        (unless (hym/workspace--server-live-p key repo)
          (hym/workspace--start-server ws repo)
          (push repo started)))
      (if started
          (message "Started %d server%s" (length started)
                   (if (= (length started) 1) "" "s"))
        (message "All workspace servers are already running")))))

(defun hym/workspace-restart-running-servers ()
  "Restart every live server in the current workspace.
Servers that are not currently running are left stopped."
  (interactive)
  (when-let* ((ws (hym/workspace-current)))
    (let ((servers (hym/workspace--live-servers (hym/workspace--key ws))))
      (unless servers
        (user-error "No servers are running in this workspace"))
      (cl-loop for server in servers
               for delay from 0 by hym/workspace-restart-server-delay
               do (let ((repo (hym/workspace-server-repo server))
                        (delay delay))
                    (hym/workspace--stop-server
                     server t
                     (lambda ()
                       (run-at-time delay nil
                                    #'hym/workspace--start-server ws repo)))))
      (hym/workspace-refresh-ui)
      (message "Restarting %d server%s" (length servers)
               (if (= (length servers) 1) "" "s")))))

(provide 'hym/workspaces-server)

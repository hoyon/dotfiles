;; -*- lexical-binding: t -*-

(require 'ert)
(defmacro hym/leader-def (&rest _))
(defmacro general-define-key (&rest _))
(load-file (expand-file-name "git-delta.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-server.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-run.el" (file-name-directory load-file-name)))
(defvar ghostel-environment nil)

(defun hym/workspace-server-test--track (workspace repo buf)
  "Track a local server for REPO in WORKSPACE whose buffer is BUF."
  (let ((server (hym/workspace-local-server-create
                 :workspace workspace :repo repo :buffer (buffer-name buf))))
    (puthash (list workspace repo) server hym/workspace--servers)
    server))

(defun hym/workspace-server-test--start (name buf)
  "Start a long-running process NAME in BUF that never blocks Emacs exit."
  (let ((proc (start-process name buf "sleep" "30")))
    (set-process-query-on-exit-flag proc nil)
    proc))

(cl-defstruct (hym/workspace-server-test--fake
               (:include hym/workspace-server)
               (:constructor hym/workspace-server-test--fake-create))
  "An always-live server on HOST that records stops in STOPPED."
  host stopped)

(cl-defmethod hym/workspace-server-host ((server hym/workspace-server-test--fake))
  (hym/workspace-server-test--fake-host server))

(cl-defmethod hym/workspace-server-live-p ((server hym/workspace-server-test--fake))
  (not (hym/workspace-server-test--fake-stopped server)))

(cl-defmethod hym/workspace-server-stop ((server hym/workspace-server-test--fake)
                                         after-stop)
  (setf (hym/workspace-server-test--fake-stopped server) t)
  (when after-stop (funcall after-stop)))

(ert-deftest hym/workspace-server-badge-tracks-multiple-live-processes ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (api-buf (generate-new-buffer " *api-srv-test*"))
        (web-buf (generate-new-buffer " *web-srv-test*")))
    (unwind-protect
        (progn
          (should (null (hym/workspace--server-badge ws)))
          (let ((api-proc (hym/workspace-server-test--start "api-srv" api-buf))
                (web-proc (hym/workspace-server-test--start "web-srv" web-buf)))
            (hym/workspace-server-test--track "s" "api" api-buf)
            (hym/workspace-server-test--track "s" "web" web-buf)
            (should (equal '("● api running" "● web running")
                           (mapcar #'substring-no-properties
                                   (hym/workspace--server-badge ws))))
            (delete-process api-proc)
            (should (equal '("● web running")
                           (mapcar #'substring-no-properties
                                   (hym/workspace--server-badge ws))))
            (should (null (hym/workspace--server "s" "api")))
            (delete-process web-proc))
          (should (null (hym/workspace--server-badge ws))))
      (kill-buffer api-buf)
      (kill-buffer web-buf))))

(ert-deftest hym/workspace-server-live-p-is-scoped-by-repo ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *srv-live-test*")))
    (unwind-protect
        (let ((proc (hym/workspace-server-test--start "srv-live" buf)))
          (hym/workspace-server-test--track "s" "api" buf)
          (should (hym/workspace--server-live-p "s" "api"))
          (should-not (hym/workspace--server-live-p "s" "web"))
          (should-not (hym/workspace--server-live-p "other" "api"))
          (delete-process proc))
      (kill-buffer buf))))

(ert-deftest hym/workspace-stop-server-stops-only-the-selected-repo ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (api-buf (generate-new-buffer " *api-kill-test*"))
        (web-buf (generate-new-buffer " *web-kill-test*"))
        (scheduled nil)
        (orig-run-at-time (symbol-function 'run-at-time)))
    (unwind-protect
        (progn
          (fset 'run-at-time
                (lambda (delay _repeat function proc buf &optional after-kill)
                  (setq scheduled
                        (list delay function proc buf after-kill))))
          (let ((api-proc (hym/workspace-server-test--start "api-kill" api-buf))
                (web-proc (hym/workspace-server-test--start "web-kill" web-buf))
                (api (hym/workspace-server-test--track "s" "api" api-buf))
                (web (hym/workspace-server-test--track "s" "web" web-buf)))
            (hym/workspace--stop-server api)
            (should-not (buffer-live-p api-buf))
            (should (eq #'ignore (process-filter api-proc)))
            (should (eq #'ignore (process-sentinel api-proc)))
            (should-not (process-buffer api-proc))
            (should (equal hym/workspace-server-shutdown-timeout
                           (car scheduled)))
            (should (eq #'hym/workspace--finish-killing-server
                        (nth 1 scheduled)))
            (should (eq api-proc (nth 2 scheduled)))
            (should (eq api-buf (nth 3 scheduled)))
            (should (process-live-p web-proc))
            (should (buffer-live-p web-buf))
            (should (null (hym/workspace--server "s" "api")))
            (should (eq web (hym/workspace--server "s" "web")))
            (delete-process api-proc)
            (delete-process web-proc)))
      (fset 'run-at-time orig-run-at-time)
      (when (buffer-live-p api-buf) (kill-buffer api-buf))
      (when (buffer-live-p web-buf) (kill-buffer web-buf)))))

(ert-deftest hym/workspace-stop-server-keeps-a-replacement-started-synchronously ()
  (let* ((hym/workspace--servers (make-hash-table :test 'equal))
         (hym/workspace-ui-refresh-hook nil)
         (old (hym/workspace-server-test--fake-create :workspace "s" :repo "api"))
         (new (hym/workspace-server-test--fake-create :workspace "s" :repo "api")))
    (puthash '("s" "api") old hym/workspace--servers)
    (hym/workspace--stop-server
     old nil (lambda () (puthash '("s" "api") new hym/workspace--servers)))
    (should (eq new (hym/workspace--server "s" "api")))))

(ert-deftest hym/workspace-kill-workspace-servers-kills-all-live-for-workspace ()
  (let* ((stopped nil)
         (refreshed nil)
         (ws '(:name "w" :slug "s" :type worktree :root "~"))
         (servers (list (hym/workspace-server-test--fake-create
                         :workspace "s" :repo "api")
                        (hym/workspace-server-test--fake-create
                         :workspace "s" :repo "worker")))
         (hym/workspace-ui-refresh-hook nil)
         (orig-live (symbol-function 'hym/workspace--live-servers))
         (orig-stop (symbol-function 'hym/workspace--stop-server)))
    (unwind-protect
        (progn
          (fset 'hym/workspace--live-servers (lambda (_key) servers))
          (fset 'hym/workspace--stop-server
                (lambda (server &optional defer-refresh _after-stop)
                  (push (list (hym/workspace-server-repo server) defer-refresh)
                        stopped)))
          (add-hook 'hym/workspace-ui-refresh-hook
                    (lambda () (setq refreshed t)))
          (should (equal '("api" "worker")
                         (hym/workspace-kill-workspace-servers ws)))
          (should (equal '(("api" t) ("worker" t))
                         (sort stopped
                               (lambda (a b) (string< (car a) (car b))))))
          (should refreshed))
      (fset 'hym/workspace--live-servers orig-live)
      (fset 'hym/workspace--stop-server orig-stop))))

(ert-deftest hym/workspace-kill-server-picker-includes-all-workspaces ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (api-buf (generate-new-buffer " *api-picker-test*"))
        (web-buf (generate-new-buffer " *web-picker-test*")))
    (unwind-protect
        (let ((api-proc (hym/workspace-server-test--start "api-picker" api-buf)))
          (hym/workspace-server-test--start "web-picker" web-buf)
          (hym/workspace-server-test--track "alpha" "api" api-buf)
          (hym/workspace-server-test--track "beta" "web" web-buf)
          (let ((choices (hym/workspace--running-server-choices)))
            (should (equal '("alpha/api" "beta/web")
                           (mapcar #'car choices)))
            (hym/workspace--stop-server (cdr (assoc "beta/web" choices))))
          (should (process-live-p api-proc))
          (should (buffer-live-p api-buf))
          (should-not (buffer-live-p web-buf))
          (delete-process api-proc))
      (when (buffer-live-p api-buf) (kill-buffer api-buf))
      (when (buffer-live-p web-buf) (kill-buffer web-buf)))))

(ert-deftest hym/workspace-run-all-servers-starts-only-stopped-repos ()
  (let ((started nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (orig-current (symbol-function 'hym/workspace-current))
        (orig-repos (symbol-function 'hym/workspace--repos-with-run))
        (orig-live (symbol-function 'hym/workspace--server-live-p))
        (orig-start (symbol-function 'hym/workspace--start-server)))
    (unwind-protect
        (progn
          (fset 'hym/workspace-current (lambda () ws))
          (fset 'hym/workspace--repos-with-run
                (lambda (_ws) '("api" "web" "worker")))
          (fset 'hym/workspace--server-live-p
                (lambda (_key repo) (equal repo "web")))
          (fset 'hym/workspace--start-server
                (lambda (_ws repo) (push repo started)))
          (hym/workspace-run-all-servers)
          (should (equal '("api" "worker") (sort started #'string<))))
      (fset 'hym/workspace-current orig-current)
      (fset 'hym/workspace--repos-with-run orig-repos)
      (fset 'hym/workspace--server-live-p orig-live)
      (fset 'hym/workspace--start-server orig-start))))

(ert-deftest hym/workspace-restart-running-servers-preserves-stopped-set ()
  (let ((stopped nil)
        (scheduled nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (servers (list (hym/workspace-server-test--fake-create
                        :workspace "s" :repo "api")
                       (hym/workspace-server-test--fake-create
                        :workspace "s" :repo "worker")))
        (orig-current (symbol-function 'hym/workspace-current))
        (orig-live (symbol-function 'hym/workspace--live-servers))
        (orig-stop (symbol-function 'hym/workspace--stop-server))
        (orig-start (symbol-function 'hym/workspace--start-server))
        (orig-run-at-time (symbol-function 'run-at-time)))
    (unwind-protect
        (progn
          (fset 'hym/workspace-current (lambda () ws))
          (fset 'hym/workspace--live-servers (lambda (_key) servers))
          (fset 'hym/workspace--stop-server
                (lambda (server &optional _defer-refresh after-stop)
                  (push (hym/workspace-server-repo server) stopped)
                  (funcall after-stop)))
          (fset 'hym/workspace--start-server
                (lambda (_ws _repo)))
          (fset 'run-at-time
                (lambda (delay _repeat function ws repo)
                  (push (list delay function ws repo) scheduled)))
          (hym/workspace-restart-running-servers)
          (should (equal '("api" "worker") (sort stopped #'string<)))
          (should (equal '("api" "worker")
                         (sort (mapcar (lambda (entry) (nth 3 entry))
                                       scheduled)
                               #'string<)))
          (should (seq-every-p
                   (lambda (entry)
                     (and (eq (nth 1 entry) #'hym/workspace--start-server)
                          (eq (nth 2 entry) ws)))
                   scheduled)))
      (fset 'hym/workspace-current orig-current)
      (fset 'hym/workspace--live-servers orig-live)
      (fset 'hym/workspace--stop-server orig-stop)
      (fset 'hym/workspace--start-server orig-start)
      (fset 'run-at-time orig-run-at-time))))

(ert-deftest hym/workspace-repos-with-run-filters ()
  (let ((root (make-temp-file "hym-code" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "a" root))
          (with-temp-file (expand-file-name "a/conductor.json" root)
            (insert "{\"scripts\":{\"run\":\"npm run dev\"}}"))
          (make-directory (expand-file-name "b" root))
          (with-temp-file (expand-file-name "b/conductor.json" root)
            (insert "{\"scripts\":{\"setup\":\"x\"}}"))
          (let ((hym/workspace-code-root root)
                (ws '(:name "w" :slug "s" :type worktree :root "~"
                      :repos ("a" "b"))))
            (should (equal (hym/workspace--repos-with-run ws) '("a")))))
      (delete-directory root t))))

(ert-deftest hym/workspace-server-environment-reads-repo-mapping ()
  (let ((file (make-temp-file "hym-server-environments")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "((\"api\" . (\"PORT=4101\" \"FEATURE=true\")))"))
          (let ((hym/workspace-server-environments-file file))
            (should (equal (hym/workspace--server-environment "api")
                           '("PORT=4101" "FEATURE=true")))
            (should-not (hym/workspace--server-environment "web"))))
      (delete-file file))))

(ert-deftest hym/workspace-server-environment-rejects-malformed-values ()
  (let ((file (make-temp-file "hym-server-environments")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "((\"api\" . \"PORT=4101\"))"))
          (let ((hym/workspace-server-environments-file file))
            (should-error (hym/workspace--server-environment "api")
                          :type 'error)))
      (delete-file file))))

(ert-deftest hym/workspace-start-server-reuses-its-tab ()
  (let ((spawned nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (hym/workspace--servers (make-hash-table :test 'equal))
        (orig-conductor (symbol-function 'hym/workspace--repo-conductor))
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab)))
    (unwind-protect
        (progn
          (fset 'hym/workspace--repo-conductor
                (lambda (_dir) '((run . "npm run dev"))))
          (fset 'hym/workspace-spawn-tab
                (lambda (_ws name _setup &optional reuse)
                  (setq spawned (list name reuse))))
          (hym/workspace--start-server ws "api")
          (should (equal spawned '("server:api" t))))
      (fset 'hym/workspace--repo-conductor orig-conductor)
      (fset 'hym/workspace-spawn-tab orig-spawn))))

(ert-deftest hym/workspace-start-server-evicts-same-repo-in-other-workspaces ()
  (let ((events nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (hym/workspace--servers (make-hash-table :test 'equal))
        (orig-stop (symbol-function 'hym/workspace--stop-server))
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab)))
    (unwind-protect
        (progn
          (dolist (id '(("beta" "api") ("beta" "web")))
            (puthash id (hym/workspace-server-test--fake-create
                         :workspace (car id) :repo (cadr id))
                     hym/workspace--servers))
          (fset 'hym/workspace--stop-server
                (lambda (server &optional _defer-refresh after-stop)
                  (push (list 'stop (hym/workspace--server-label server)) events)
                  (funcall after-stop)))
          (fset 'hym/workspace-spawn-tab
                (lambda (_ws name _setup &optional _reuse)
                  (push (list 'spawn name) events)))
          (hym/workspace--start-server ws "api")
          (should (equal (nreverse events)
                         '((stop "beta/api") (spawn "server:api")))))
      (fset 'hym/workspace--stop-server orig-stop)
      (fset 'hym/workspace-spawn-tab orig-spawn))))

(ert-deftest hym/workspace-conflicting-servers-are-same-repo-same-host ()
  (let* ((hym/workspace--servers (make-hash-table :test 'equal))
         (mine (hym/workspace-server-test--fake-create
                :workspace "s" :repo "api"))
         (local-api (hym/workspace-server-test--fake-create
                     :workspace "a" :repo "api"))
         (remote-api (hym/workspace-server-test--fake-create
                      :workspace "b" :repo "api" :host "devbox"))
         (local-web (hym/workspace-server-test--fake-create
                     :workspace "c" :repo "web")))
    (dolist (server (list mine local-api remote-api local-web))
      (puthash (hym/workspace--server-id server) server hym/workspace--servers))
    (should (equal (list local-api) (hym/workspace--conflicting-servers mine)))))

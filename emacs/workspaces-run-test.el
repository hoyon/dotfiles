;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-run.el" (file-name-directory load-file-name)))
(defvar ghostel-environment nil)

(ert-deftest hym-workspace-agent-signal-maps-events ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal)))
    (hym-workspace-agent-signal "s" "claude" "one" "UserPromptSubmit")
    (should (eq 'working
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "claude" "one" "Stop")
    (should (eq 'waiting
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    ;; Notification fires for idle_prompt/agent_completed too, so it must not
    ;; clobber the waiting badge into "needs permission".
    (hym-workspace-agent-signal "s" "claude" "one" "Notification")
    (should (eq 'waiting
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "claude" "one" "PermissionRequest")
    (should (eq 'permission
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "claude" "one" "agent_needs_input")
    (should (eq 'question
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "claude" "one" "SessionEnd")
    (should (null (gethash '("s" "one") hym-workspace--agent-state)))))

(ert-deftest hym-workspace-agent-signal-keeps-sessions-separate ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal)))
    (hym-workspace-agent-signal "s" "codex" "one" "UserPromptSubmit")
    (hym-workspace-agent-signal "s" "claude" "two" "PermissionRequest")
    (should (eq 'working
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (should (eq 'permission
                (plist-get (gethash '("s" "two") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "codex" "one" "SessionEnd")
    (should (null (gethash '("s" "one") hym-workspace--agent-state)))
    (should (eq 'permission
                (plist-get (gethash '("s" "two") hym-workspace--agent-state)
                           :state)))))

(ert-deftest hym-workspace-agent-signal-refreshes-only-on-change ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (n 0)
        (orig (symbol-function 'hym-workspace--run-refresh)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--run-refresh (lambda () (setq n (1+ n))))
          (hym-workspace-agent-signal "s" "claude" "one" "Stop")
          (hym-workspace-agent-signal "s" "claude" "one" "Stop")
          (hym-workspace-agent-signal "s" "claude" "one" "PostToolUse")
          (hym-workspace-agent-signal "s" "claude" "one" "PreToolUse")
          (should (= n 2)))
      (fset 'hym-workspace--run-refresh orig))))

(ert-deftest hym-workspace-agent-signal-refreshes-timestamp-without-rerender ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (n 0)
        (orig (symbol-function 'hym-workspace--run-refresh)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--run-refresh (lambda () (setq n (1+ n))))
          (hym-workspace-agent-signal "s" "claude" "one" "UserPromptSubmit")
          (let* ((entry (gethash '("s" "one") hym-workspace--agent-state))
                 (first (plist-get entry :updated-at)))
            (plist-put entry :updated-at (- first 10))
            (puthash '("s" "one") entry hym-workspace--agent-state)
            (hym-workspace-agent-signal "s" "claude" "one" "PreToolUse")
            (should (= n 1))
            (should (> (plist-get (gethash '("s" "one")
                                            hym-workspace--agent-state)
                                  :updated-at)
                       first))))
      (fset 'hym-workspace--run-refresh orig))))

(ert-deftest hym-workspace-agent-badge-reflects-state ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (should (null (hym-workspace--agent-badge ws)))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (float-time))
             hym-workspace--agent-state)
    (should (string-match-p "running" (car (hym-workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'waiting :updated-at (float-time))
             hym-workspace--agent-state)
    (should (string-match-p "waiting" (car (hym-workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'question :updated-at (float-time))
             hym-workspace--agent-state)
    (should (string-match-p "needs input" (car (hym-workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'permission :updated-at (float-time))
             hym-workspace--agent-state)
    (should (string-match-p "permission" (car (hym-workspace--agent-badge ws))))))

(ert-deftest hym-workspace-agent-badge-lists-multiple-sessions ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (float-time))
             hym-workspace--agent-state)
    (puthash '("s" "two")
             (list :slug "s" :agent "claude" :session "two"
                   :state 'waiting :updated-at (float-time))
             hym-workspace--agent-state)
    (let ((badges (hym-workspace--agent-badge ws)))
      (should (= 2 (length badges)))
      (should (seq-some (lambda (line) (string-match-p "codex running" line))
                        badges))
      (should (seq-some (lambda (line) (string-match-p "claude waiting" line))
                        badges)))))

(ert-deftest hym-workspace-agent-badge-accepts-legacy-state ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (puthash "s" 'waiting hym-workspace--agent-state)
    (should (string-match-p "agent waiting"
                            (car (hym-workspace--agent-badge ws))))
    (hym-workspace-agent-signal "s" "SessionEnd")
    (should (null (gethash "s" hym-workspace--agent-state)))))

(ert-deftest hym-workspace-agent-badge-clears-stale-working-only ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (hym-workspace-agent-working-timeout 1)
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (- (float-time) 10))
             hym-workspace--agent-state)
    (should (null (hym-workspace--agent-badge ws)))
    (should (null (gethash '("s" "one") hym-workspace--agent-state)))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'waiting :updated-at (- (float-time) 10))
             hym-workspace--agent-state)
    (should (string-match-p "waiting" (car (hym-workspace--agent-badge ws))))))

(ert-deftest hym-workspace-server-badge-tracks-multiple-live-processes ()
  (let ((hym-workspace--servers (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (api-buf (generate-new-buffer " *api-srv-test*"))
        (web-buf (generate-new-buffer " *web-srv-test*")))
    (unwind-protect
        (progn
          (should (null (hym-workspace--server-badge ws)))
          (let ((api-proc (start-process "api-srv" api-buf "sleep" "30"))
                (web-proc (start-process "web-srv" web-buf "sleep" "30")))
            (puthash '("s" "api") (buffer-name api-buf) hym-workspace--servers)
            (puthash '("s" "web") (buffer-name web-buf) hym-workspace--servers)
            (should (equal '("● api server running" "● web server running")
                           (mapcar #'substring-no-properties
                                   (hym-workspace--server-badge ws))))
            (delete-process api-proc)
            (should (equal '("● web server running")
                           (mapcar #'substring-no-properties
                                   (hym-workspace--server-badge ws))))
            (should (null (gethash '("s" "api") hym-workspace--servers)))
            (delete-process web-proc))
          (should (null (hym-workspace--server-badge ws))))
      (kill-buffer api-buf)
      (kill-buffer web-buf))))

(ert-deftest hym-workspace-server-live-p-is-scoped-by-repo ()
  (let ((hym-workspace--servers (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *srv-live-test*")))
    (unwind-protect
        (let ((proc (start-process "srv-live" buf "sleep" "30")))
          (puthash '("s" "api") (buffer-name buf) hym-workspace--servers)
          (should (hym-workspace--server-live-p "s" "api"))
          (should-not (hym-workspace--server-live-p "s" "web"))
          (should-not (hym-workspace--server-live-p "other" "api"))
          (delete-process proc))
      (kill-buffer buf))))

(ert-deftest hym-workspace-kill-server-stops-only-the-selected-repo ()
  (let ((hym-workspace--servers (make-hash-table :test 'equal))
        (api-buf (generate-new-buffer " *api-kill-test*"))
        (web-buf (generate-new-buffer " *web-kill-test*")))
    (unwind-protect
        (let ((api-proc (start-process "api-kill" api-buf "sleep" "30"))
              (web-proc (start-process "web-kill" web-buf "sleep" "30")))
          (puthash '("s" "api") (buffer-name api-buf) hym-workspace--servers)
          (puthash '("s" "web") (buffer-name web-buf) hym-workspace--servers)
          (hym-workspace--kill-server "s" "api")
          (should-not (process-live-p api-proc))
          (should (process-live-p web-proc))
          (should (null (gethash '("s" "api") hym-workspace--servers)))
          (should (equal (buffer-name web-buf)
                         (gethash '("s" "web") hym-workspace--servers)))
          (delete-process web-proc))
      (kill-buffer api-buf)
      (kill-buffer web-buf))))

(ert-deftest hym-workspace-kill-workspace-servers-kills-all-live-for-workspace ()
  (let ((killed nil)
        (refreshed nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (orig-live (symbol-function 'hym-workspace--live-servers))
        (orig-kill (symbol-function 'hym-workspace--kill-server))
        (orig-refresh (symbol-function 'hym-workspace--run-refresh)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--live-servers
                (lambda (_key) '(("api" . "api-buffer")
                                 ("worker" . "worker-buffer"))))
          (fset 'hym-workspace--kill-server
                (lambda (_key repo &optional defer-refresh)
                  (push (list repo defer-refresh) killed)))
          (fset 'hym-workspace--run-refresh
                (lambda () (setq refreshed t)))
          (should (equal '("api" "worker")
                         (hym-workspace-kill-workspace-servers ws)))
          (should (equal '(("api" t) ("worker" t))
                         (sort killed
                               (lambda (a b) (string< (car a) (car b))))))
          (should refreshed))
      (fset 'hym-workspace--live-servers orig-live)
      (fset 'hym-workspace--kill-server orig-kill)
      (fset 'hym-workspace--run-refresh orig-refresh))))

(ert-deftest hym-workspace-kill-server-picker-includes-all-workspaces ()
  (let ((hym-workspace--servers (make-hash-table :test 'equal))
        (api-buf (generate-new-buffer " *api-picker-test*"))
        (web-buf (generate-new-buffer " *web-picker-test*")))
    (unwind-protect
        (let ((api-proc (start-process "api-picker" api-buf "sleep" "30"))
              (web-proc (start-process "web-picker" web-buf "sleep" "30")))
          (set-process-query-on-exit-flag api-proc nil)
          (set-process-query-on-exit-flag web-proc nil)
          (puthash '("alpha" "api") (buffer-name api-buf)
                   hym-workspace--servers)
          (puthash '("beta" "web") (buffer-name web-buf)
                   hym-workspace--servers)
          (let* ((choices (hym-workspace--running-server-choices))
                 (server-key (cdr (assoc "beta/web" choices))))
            (should (equal '("alpha/api" "beta/web")
                           (mapcar #'car choices)))
            (hym-workspace--kill-server (car server-key) (cadr server-key)))
          (should (process-live-p api-proc))
          (should-not (process-live-p web-proc))
          (delete-process api-proc))
      (kill-buffer api-buf)
      (kill-buffer web-buf))))

(ert-deftest hym-workspace-rename-server-tab-marks-old-tab ()
  (let* ((buf (generate-new-buffer " *api-tab-rename-test*"))
         (tab '((name . "server:api")))
         (renamed nil)
         (tab-bar-tabs-function (lambda () (list tab)))
         (orig-get-tab (symbol-function 'tab-bar-get-buffer-tab))
         (orig-rename (symbol-function 'tab-bar-rename-tab)))
    (unwind-protect
        (progn
          (fset 'tab-bar-get-buffer-tab
                (lambda (_buf &rest _args) (list tab)))
          (fset 'tab-bar-rename-tab
                (lambda (name &optional tab-number)
                  (setq renamed (list name tab-number))))
          (hym-workspace--rename-server-tab buf)
          (should (equal renamed '("old:server:api" 1))))
      (fset 'tab-bar-get-buffer-tab orig-get-tab)
      (fset 'tab-bar-rename-tab orig-rename)
      (kill-buffer buf))))

(ert-deftest hym-workspace-rename-server-tab-marks-every-containing-tab ()
  (let* ((buf (generate-new-buffer " *api-tab-rename-test*"))
         (api-tab '((name . "server:api")))
         (web-tab '((name . "server:web")))
         (file-tab '((name . "files")))
         (renamed nil)
         (tab-bar-tabs-function (lambda () (list file-tab api-tab web-tab)))
         (orig-get-tab (symbol-function 'tab-bar-get-buffer-tab))
         (orig-rename (symbol-function 'tab-bar-rename-tab)))
    (unwind-protect
        (progn
          (fset 'tab-bar-get-buffer-tab
                (lambda (_buf &rest _args) (list api-tab web-tab)))
          (fset 'tab-bar-rename-tab
                (lambda (name &optional tab-number)
                  (push (list name tab-number) renamed)))
          (hym-workspace--rename-server-tab buf)
          (should (equal (sort renamed (lambda (a b) (< (cadr a) (cadr b))))
                         '(("old:server:api" 2)
                           ("old:server:web" 3)))))
      (fset 'tab-bar-get-buffer-tab orig-get-tab)
      (fset 'tab-bar-rename-tab orig-rename)
      (kill-buffer buf))))

(ert-deftest hym-workspace-rename-server-buffer-frees-active-name ()
  (let ((buf (generate-new-buffer "*ws-server: s/api*")))
    (unwind-protect
        (progn
          (hym-workspace--rename-server-buffer buf)
          (should (string-prefix-p "*old:ws-server: s/api"
                                   (buffer-name buf))))
      (kill-buffer buf))))

(ert-deftest hym-workspace-run-all-servers-starts-only-stopped-repos ()
  (let ((started nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (orig-current (symbol-function 'hym-workspace-current))
        (orig-repos (symbol-function 'hym-workspace--repos-with-run))
        (orig-live (symbol-function 'hym-workspace--server-live-p))
        (orig-start (symbol-function 'hym-workspace--start-server)))
    (unwind-protect
        (progn
          (fset 'hym-workspace-current (lambda () ws))
          (fset 'hym-workspace--repos-with-run
                (lambda (_ws) '("api" "web" "worker")))
          (fset 'hym-workspace--server-live-p
                (lambda (_key repo) (equal repo "web")))
          (fset 'hym-workspace--start-server
                (lambda (_ws repo) (push repo started)))
          (hym-workspace-run-all-servers)
          (should (equal '("api" "worker") (sort started #'string<))))
      (fset 'hym-workspace-current orig-current)
      (fset 'hym-workspace--repos-with-run orig-repos)
      (fset 'hym-workspace--server-live-p orig-live)
      (fset 'hym-workspace--start-server orig-start))))

(ert-deftest hym-workspace-restart-running-servers-preserves-stopped-set ()
  (let ((killed nil)
        (scheduled nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (orig-current (symbol-function 'hym-workspace-current))
        (orig-live (symbol-function 'hym-workspace--live-servers))
        (orig-kill (symbol-function 'hym-workspace--kill-server))
        (orig-start (symbol-function 'hym-workspace--start-server))
        (orig-run-at-time (symbol-function 'run-at-time)))
    (unwind-protect
        (progn
          (fset 'hym-workspace-current (lambda () ws))
          (fset 'hym-workspace--live-servers
                (lambda (_key) '(("api" . "api-buffer")
                                 ("worker" . "worker-buffer"))))
          (fset 'hym-workspace--kill-server
                (lambda (_key repo &optional _defer-refresh) (push repo killed)))
          (fset 'hym-workspace--start-server
                (lambda (_ws _repo)))
          (fset 'run-at-time
                (lambda (delay _repeat function ws repo)
                  (push (list delay function ws repo) scheduled)))
          (hym-workspace-restart-running-servers)
          (should (equal '("api" "worker") (sort killed #'string<)))
          (should (equal '("api" "worker")
                         (sort (mapcar (lambda (entry) (nth 3 entry))
                                       scheduled)
                               #'string<)))
          (should (seq-every-p
                   (lambda (entry)
                     (and (eq (nth 1 entry) #'hym-workspace--start-server)
                          (eq (nth 2 entry) ws)))
                   scheduled)))
      (fset 'hym-workspace-current orig-current)
      (fset 'hym-workspace--live-servers orig-live)
      (fset 'hym-workspace--kill-server orig-kill)
      (fset 'hym-workspace--start-server orig-start)
      (fset 'run-at-time orig-run-at-time))))

(ert-deftest hym-workspace-repos-with-run-filters ()
  (let ((root (make-temp-file "hym-code" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "a" root))
          (with-temp-file (expand-file-name "a/conductor.json" root)
            (insert "{\"scripts\":{\"run\":\"npm run dev\"}}"))
          (make-directory (expand-file-name "b" root))
          (with-temp-file (expand-file-name "b/conductor.json" root)
            (insert "{\"scripts\":{\"setup\":\"x\"}}"))
          (let ((hym-workspace-code-root root)
                (ws '(:name "w" :slug "s" :type worktree :root "~"
                      :repos ("a" "b"))))
            (should (equal (hym-workspace--repos-with-run ws) '("a")))))
      (delete-directory root t))))

(ert-deftest hym-workspace-agent-env-carries-slug-and-name ()
  (let ((env (hym-workspace--agent-env '(:name "w" :slug "auth" :type worktree :root "~")
                                       "claude" "session-1")))
    (should (member "HYM_WORKSPACE_SLUG=auth" env))
    (should (member "HYM_WORKSPACE_AGENT=claude" env))
    (should (member "HYM_WORKSPACE_AGENT_SESSION=session-1" env))))

(ert-deftest hym-workspace-agent-env-uses-key-for-slugless-workspace ()
  (let ((env (hym-workspace--agent-env '(:name "Dot Files" :type project :root "~")
                                       "claude" "session-1")))
    (should (member "HYM_WORKSPACE_SLUG=dot_files" env))
    (should-not (member "HYM_WORKSPACE_SLUG=nil" env))))

(ert-deftest hym-workspace-pick-agent-single-skips-prompt ()
  (let ((hym-workspace-agents '(("claude" . "claude"))))
    (should (equal (hym-workspace--pick-agent) '("claude" . "claude")))))

(ert-deftest hym-workspace-shell-quote-is-fish-safe ()
  (should (equal (hym-workspace--shell-quote "hello world") "'hello world'"))
  (should (equal (hym-workspace--shell-quote "it's big") "'it'\\''s big'")))

(ert-deftest hym-workspace-agent-launch-string-seeds-prompt ()
  (should (equal (hym-workspace--agent-launch-string "claude" nil) "claude"))
  (should (equal (hym-workspace--agent-launch-string "claude" "   ") "claude"))
  (should (equal (hym-workspace--agent-launch-string "claude" "fix it")
                 "claude 'fix it'")))

(ert-deftest hym-workspace-run-agent-shell-uses-workspace-context ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (captured-dir nil)
        (captured-env nil)
        (orig-agent-shell (and (fboundp 'agent-shell-new-shell)
                               (symbol-function 'agent-shell-new-shell)))
        (orig-current (symbol-function 'hym-workspace-current))
        (orig-spawn (symbol-function 'hym-workspace-spawn-tab)))
    (unwind-protect
        (progn
          (fset 'agent-shell-new-shell
                (lambda ()
                  (setq captured-dir default-directory)
                  (setq captured-env process-environment)))
          (fset 'hym-workspace-current
                (lambda () '(:name "Dot Files" :type project :root "/tmp/dotfiles")))
          (fset 'hym-workspace-spawn-tab
                (lambda (_ws _name setup) (funcall setup)))
          (hym-workspace-run-agent-shell)
          (should (equal captured-dir "/tmp/dotfiles"))
          (should (member "HYM_WORKSPACE_SLUG=dot_files" captured-env))
          (should (member "HYM_WORKSPACE_AGENT=agent-shell" captured-env))
          (should (seq-some
                   (lambda (env)
                     (string-prefix-p "HYM_WORKSPACE_AGENT_SESSION=agent-shell-"
                                      env))
                   captured-env)))
      (if orig-agent-shell
          (fset 'agent-shell-new-shell orig-agent-shell)
        (fmakunbound 'agent-shell-new-shell))
      (fset 'hym-workspace-current orig-current)
      (fset 'hym-workspace-spawn-tab orig-spawn))))

(ert-deftest hym-workspace-start-agent-seeds-prompt ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (sent nil)
        (captured-dir nil)
        (captured-env nil)
        (orig-spawn (symbol-function 'hym-workspace-spawn-tab)))
    (unwind-protect
        (progn
          (fset 'hym-workspace-spawn-tab (lambda (_ws _name setup) (funcall setup)))
          (makunbound 'ghostel-environment)
          (fset 'ghostel (lambda (&optional _fresh)
                           (setq captured-dir default-directory
                                 captured-env ghostel-environment)))
          (fset 'ghostel-send-string (lambda (s) (setq sent s)))
          (hym-workspace--start-agent
           '(:name "w" :slug "s" :type worktree :root "/tmp/w")
           "claude" "claude" "sess-1" "it's big")
          (should (equal captured-dir "/tmp/w"))
          (should (member "HYM_WORKSPACE_SLUG=s" captured-env))
          (should (member "HYM_WORKSPACE_AGENT=claude" captured-env))
          (should (member "HYM_WORKSPACE_AGENT_SESSION=sess-1" captured-env))
          (should (equal sent "claude 'it'\\''s big'\n")))
      (fset 'hym-workspace-spawn-tab orig-spawn)
      (fmakunbound 'ghostel)
      (fmakunbound 'ghostel-send-string))))

(ert-deftest hym-workspace-new-from-preset-starts-agent-on-success ()
  (let* ((tmp (make-temp-file "hym-preset" t))
         (ws (list :name "fix it" :slug "fix_it" :type 'worktree
                   :root tmp :repos '("ploy-client") :base-branch "main"))
         (started nil)
         (provision-ok t)
         (err-shown nil)
         (hym-workspace--loaded t)
         (hym-workspace--registry nil)
         (orig-reg (symbol-function 'hym-workspace--register-worktree))
         (orig-open (symbol-function 'hym-workspace-open))
         (orig-prov (symbol-function 'hym-workspace--provision))
         (orig-start (symbol-function 'hym-workspace--start-agent))
         (orig-err (symbol-function 'hym-workspace--show-setup-error)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--register-worktree (lambda (&rest _) ws))
          (fset 'hym-workspace-open (lambda (&rest _) ws))
          (fset 'hym-workspace--provision
                (lambda (_ws _repos _reuse cb) (funcall cb provision-ok)))
          (fset 'hym-workspace--start-agent
                (lambda (_ws name command &optional _session prompt)
                  (setq started (list name command prompt))))
          (fset 'hym-workspace--show-setup-error
                (lambda (&rest _) (setq err-shown t)))
          (let ((hym-workspace-agents '(("claude" . "claude"))))
            (hym-workspace-new-from-preset
             '(:name "frontend" :repos ("ploy-client") :agent "claude")
             "make the button smaller"))
          (should (equal started '("claude" "claude" "make the button smaller")))
          (should (null err-shown))

          (setq started nil provision-ok nil)
          (let ((hym-workspace-agents '(("claude" . "claude"))))
            (hym-workspace-new-from-preset
             '(:name "frontend" :repos ("ploy-client") :agent "claude")
             "make the button smaller"))
          (should (null started))
          (should err-shown))
      (fset 'hym-workspace--register-worktree orig-reg)
      (fset 'hym-workspace-open orig-open)
      (fset 'hym-workspace--provision orig-prov)
      (fset 'hym-workspace--start-agent orig-start)
      (fset 'hym-workspace--show-setup-error orig-err)
      (delete-directory tmp t))))

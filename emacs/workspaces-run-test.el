;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-run.el" (file-name-directory load-file-name)))
(defvar ghostel-environment nil)
(defvar ghostel-buffer-name "*ghostel*")

(ert-deftest hym/workspace-agent-signal-maps-events ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal)))
    (hym/workspace-agent-signal "s" "claude" "one" "UserPromptSubmit")
    (should (eq 'working
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    (hym/workspace-agent-signal "s" "claude" "one" "Stop")
    (should (eq 'waiting
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    ;; Notification fires for idle_prompt/agent_completed too, so it must not
    ;; clobber the waiting badge into "needs permission".
    (hym/workspace-agent-signal "s" "claude" "one" "Notification")
    (should (eq 'waiting
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    (hym/workspace-agent-signal "s" "claude" "one" "PermissionRequest")
    (should (eq 'permission
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    (hym/workspace-agent-signal "s" "claude" "one" "agent_needs_input")
    (should (eq 'question
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    (hym/workspace-agent-signal "s" "claude" "one" "SessionEnd")
    (should (null (gethash '("s" "one") hym/workspace--agent-state)))))

(ert-deftest hym/workspace-agent-signal-keeps-sessions-separate ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal)))
    (hym/workspace-agent-signal "s" "codex" "one" "UserPromptSubmit")
    (hym/workspace-agent-signal "s" "claude" "two" "PermissionRequest")
    (should (eq 'working
                (plist-get (gethash '("s" "one") hym/workspace--agent-state)
                           :state)))
    (should (eq 'permission
                (plist-get (gethash '("s" "two") hym/workspace--agent-state)
                           :state)))
    (hym/workspace-agent-signal "s" "codex" "one" "SessionEnd")
    (should (null (gethash '("s" "one") hym/workspace--agent-state)))
    (should (eq 'permission
                (plist-get (gethash '("s" "two") hym/workspace--agent-state)
                           :state)))))

(ert-deftest hym/workspace-agent-signal-refreshes-only-on-change ()
  (let* ((hym/workspace--agent-state (make-hash-table :test 'equal))
         (n 0)
         (hym/workspace-ui-refresh-hook (list (lambda () (setq n (1+ n))))))
    (hym/workspace-agent-signal "s" "claude" "one" "Stop")
    (hym/workspace-agent-signal "s" "claude" "one" "Stop")
    (hym/workspace-agent-signal "s" "claude" "one" "PostToolUse")
    (hym/workspace-agent-signal "s" "claude" "one" "PreToolUse")
    (should (= n 2))))

(ert-deftest hym/workspace-agent-signal-refreshes-timestamp-without-rerender ()
  (let* ((hym/workspace--agent-state (make-hash-table :test 'equal))
         (n 0)
         (hym/workspace-ui-refresh-hook (list (lambda () (setq n (1+ n))))))
    (hym/workspace-agent-signal "s" "claude" "one" "UserPromptSubmit")
    (let* ((entry (gethash '("s" "one") hym/workspace--agent-state))
           (first (plist-get entry :updated-at)))
      (plist-put entry :updated-at (- first 10))
      (puthash '("s" "one") entry hym/workspace--agent-state)
      (hym/workspace-agent-signal "s" "claude" "one" "PreToolUse")
      (should (= n 1))
      (should (> (plist-get (gethash '("s" "one")
                                     hym/workspace--agent-state)
                            :updated-at)
                 first)))))

(ert-deftest hym/workspace-agent-badge-reflects-state ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (should (null (hym/workspace--agent-badge ws)))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (float-time))
             hym/workspace--agent-state)
    (should (string-match-p "running" (car (hym/workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'waiting :updated-at (float-time))
             hym/workspace--agent-state)
    (should (string-match-p "waiting" (car (hym/workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'question :updated-at (float-time))
             hym/workspace--agent-state)
    (should (string-match-p "needs input" (car (hym/workspace--agent-badge ws))))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'permission :updated-at (float-time))
             hym/workspace--agent-state)
    (should (string-match-p "permission" (car (hym/workspace--agent-badge ws))))))

(ert-deftest hym/workspace-agent-badge-lists-multiple-sessions ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (float-time))
             hym/workspace--agent-state)
    (puthash '("s" "two")
             (list :slug "s" :agent "claude" :session "two"
                   :state 'waiting :updated-at (float-time))
             hym/workspace--agent-state)
    (let ((badges (hym/workspace--agent-badge ws)))
      (should (= 2 (length badges)))
      (should (seq-some (lambda (line) (string-match-p "codex running" line))
                        badges))
      (should (seq-some (lambda (line) (string-match-p "claude waiting" line))
                        badges)))))

(ert-deftest hym/workspace-agent-signal-session-end-clears-only-that-session ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal)))
    (hym/workspace-agent-signal "s" "claude" "one" "Stop")
    (hym/workspace-agent-signal "s" "claude" "two" "Stop")
    (hym/workspace-agent-signal "s" "claude" "one" "SessionEnd")
    (should (null (gethash '("s" "one") hym/workspace--agent-state)))
    (should (gethash '("s" "two") hym/workspace--agent-state))))

(ert-deftest hym/workspace-agent-badge-clears-stale-working-only ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal))
        (hym/workspace-agent-working-timeout 1)
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'working :updated-at (- (float-time) 10))
             hym/workspace--agent-state)
    (should (null (hym/workspace--agent-badge ws)))
    (should (null (gethash '("s" "one") hym/workspace--agent-state)))
    (puthash '("s" "one")
             (list :slug "s" :agent "codex" :session "one"
                   :state 'waiting :updated-at (- (float-time) 10))
             hym/workspace--agent-state)
    (should (string-match-p "waiting" (car (hym/workspace--agent-badge ws))))))

(ert-deftest hym/workspace-server-badge-tracks-multiple-live-processes ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (api-buf (generate-new-buffer " *api-srv-test*"))
        (web-buf (generate-new-buffer " *web-srv-test*")))
    (unwind-protect
        (progn
          (should (null (hym/workspace--server-badge ws)))
          (let ((api-proc (start-process "api-srv" api-buf "sleep" "30"))
                (web-proc (start-process "web-srv" web-buf "sleep" "30")))
            (puthash '("s" "api") (buffer-name api-buf) hym/workspace--servers)
            (puthash '("s" "web") (buffer-name web-buf) hym/workspace--servers)
            (should (equal '("● api running" "● web running")
                           (mapcar #'substring-no-properties
                                   (hym/workspace--server-badge ws))))
            (delete-process api-proc)
            (should (equal '("● web running")
                           (mapcar #'substring-no-properties
                                   (hym/workspace--server-badge ws))))
            (should (null (gethash '("s" "api") hym/workspace--servers)))
            (delete-process web-proc))
          (should (null (hym/workspace--server-badge ws))))
      (kill-buffer api-buf)
      (kill-buffer web-buf))))

(ert-deftest hym/workspace-server-live-p-is-scoped-by-repo ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (buf (generate-new-buffer " *srv-live-test*")))
    (unwind-protect
        (let ((proc (start-process "srv-live" buf "sleep" "30")))
          (puthash '("s" "api") (buffer-name buf) hym/workspace--servers)
          (should (hym/workspace--server-live-p "s" "api"))
          (should-not (hym/workspace--server-live-p "s" "web"))
          (should-not (hym/workspace--server-live-p "other" "api"))
          (delete-process proc))
      (kill-buffer buf))))

(ert-deftest hym/workspace-kill-server-stops-only-the-selected-repo ()
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
          (let ((api-proc (start-process "api-kill" api-buf "sleep" "30"))
                (web-proc (start-process "web-kill" web-buf "sleep" "30")))
            (puthash '("s" "api") (buffer-name api-buf) hym/workspace--servers)
            (puthash '("s" "web") (buffer-name web-buf) hym/workspace--servers)
            (hym/workspace--kill-server "s" "api")
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
            (should (null (gethash '("s" "api") hym/workspace--servers)))
            (should (equal (buffer-name web-buf)
                           (gethash '("s" "web") hym/workspace--servers)))
            (delete-process web-proc)))
      (fset 'run-at-time orig-run-at-time)
      (when (buffer-live-p api-buf) (kill-buffer api-buf))
      (when (buffer-live-p web-buf) (kill-buffer web-buf)))))

(ert-deftest hym/workspace-kill-workspace-servers-kills-all-live-for-workspace ()
  (let ((killed nil)
        (refreshed nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (hym/workspace-ui-refresh-hook nil)
        (orig-live (symbol-function 'hym/workspace--live-servers))
        (orig-kill (symbol-function 'hym/workspace--kill-server)))
    (unwind-protect
        (progn
          (fset 'hym/workspace--live-servers
                (lambda (_key) '(("api" . "api-buffer")
                                 ("worker" . "worker-buffer"))))
          (fset 'hym/workspace--kill-server
                (lambda (_key repo &optional defer-refresh)
                  (push (list repo defer-refresh) killed)))
          (add-hook 'hym/workspace-ui-refresh-hook
                    (lambda () (setq refreshed t)))
          (should (equal '("api" "worker")
                         (hym/workspace-kill-workspace-servers ws)))
          (should (equal '(("api" t) ("worker" t))
                         (sort killed
                               (lambda (a b) (string< (car a) (car b))))))
          (should refreshed))
      (fset 'hym/workspace--live-servers orig-live)
      (fset 'hym/workspace--kill-server orig-kill))))

(ert-deftest hym/workspace-kill-server-picker-includes-all-workspaces ()
  (let ((hym/workspace--servers (make-hash-table :test 'equal))
        (api-buf (generate-new-buffer " *api-picker-test*"))
        (web-buf (generate-new-buffer " *web-picker-test*")))
    (unwind-protect
        (let ((api-proc (start-process "api-picker" api-buf "sleep" "30"))
              (web-proc (start-process "web-picker" web-buf "sleep" "30")))
          (set-process-query-on-exit-flag api-proc nil)
          (set-process-query-on-exit-flag web-proc nil)
          (puthash '("alpha" "api") (buffer-name api-buf)
                   hym/workspace--servers)
          (puthash '("beta" "web") (buffer-name web-buf)
                   hym/workspace--servers)
          (let* ((choices (hym/workspace--running-server-choices))
                 (server-key (cdr (assoc "beta/web" choices))))
            (should (equal '("alpha/api" "beta/web")
                           (mapcar #'car choices)))
            (hym/workspace--kill-server (car server-key) (cadr server-key)))
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
  (let ((killed nil)
        (scheduled nil)
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (orig-current (symbol-function 'hym/workspace-current))
        (orig-live (symbol-function 'hym/workspace--live-servers))
        (orig-kill (symbol-function 'hym/workspace--kill-server))
        (orig-start (symbol-function 'hym/workspace--start-server))
        (orig-run-at-time (symbol-function 'run-at-time)))
    (unwind-protect
        (progn
          (fset 'hym/workspace-current (lambda () ws))
          (fset 'hym/workspace--live-servers
                (lambda (_key) '(("api" . "api-buffer")
                                 ("worker" . "worker-buffer"))))
          (fset 'hym/workspace--kill-server
                (lambda (_key repo &optional _defer-refresh after-kill)
                  (push repo killed)
                  (funcall after-kill)))
          (fset 'hym/workspace--start-server
                (lambda (_ws _repo)))
          (fset 'run-at-time
                (lambda (delay _repeat function ws repo)
                  (push (list delay function ws repo) scheduled)))
          (hym/workspace-restart-running-servers)
          (should (equal '("api" "worker") (sort killed #'string<)))
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
      (fset 'hym/workspace--kill-server orig-kill)
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

(ert-deftest hym/workspace-agent-env-carries-slug-and-name ()
  (let ((env (hym/workspace--agent-env '(:name "w" :slug "auth" :type worktree :root "~")
                                       "claude" "session-1")))
    (should (member "HYM_WORKSPACE_SLUG=auth" env))
    (should (member "HYM_WORKSPACE_AGENT=claude" env))
    (should (member "HYM_WORKSPACE_AGENT_SESSION=session-1" env))))

(ert-deftest hym/workspace-agent-env-uses-key-for-slugless-workspace ()
  (let ((env (hym/workspace--agent-env '(:name "Dot Files" :type project :root "~")
                                       "claude" "session-1")))
    (should (member "HYM_WORKSPACE_SLUG=dot_files" env))
    (should-not (member "HYM_WORKSPACE_SLUG=nil" env))))

(ert-deftest hym/workspace-pick-agent-single-skips-prompt ()
  (let ((hym/workspace-agents '(("claude" . "claude"))))
    (should (equal (hym/workspace--pick-agent) '("claude" . "claude")))))

(ert-deftest hym/workspace-shell-quote-is-fish-safe ()
  (should (equal (hym/workspace--shell-quote "hello world") "'hello world'"))
  (should (equal (hym/workspace--shell-quote "it's big") "'it'\\''s big'")))

(ert-deftest hym/workspace-agent-launch-string-seeds-prompt ()
  (should (equal (hym/workspace--agent-launch-string "claude" nil) "claude"))
  (should (equal (hym/workspace--agent-launch-string "claude" "   ") "claude"))
  (should (equal (hym/workspace--agent-launch-string "claude" "fix it")
                 "claude 'fix it'")))

(ert-deftest hym/workspace-run-agent-shell-uses-workspace-context ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal))
        (captured-dir nil)
        (captured-env nil)
        (orig-agent-shell (and (fboundp 'agent-shell-new-shell)
                               (symbol-function 'agent-shell-new-shell)))
        (orig-current (symbol-function 'hym/workspace-current))
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab)))
    (unwind-protect
        (progn
          (fset 'agent-shell-new-shell
                (lambda ()
                  (setq captured-dir default-directory)
                  (setq captured-env process-environment)))
          (fset 'hym/workspace-current
                (lambda () '(:name "Dot Files" :type project :root "/tmp/dotfiles")))
          (fset 'hym/workspace-spawn-tab
                (lambda (_ws _name setup) (funcall setup)))
          (hym/workspace-run-agent-shell)
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
      (fset 'hym/workspace-current orig-current)
      (fset 'hym/workspace-spawn-tab orig-spawn))))

(ert-deftest hym/workspace-start-agent-seeds-prompt ()
  (let ((hym/workspace--agent-state (make-hash-table :test 'equal))
        (sent nil)
        (captured-dir nil)
        (captured-env nil)
        (captured-name nil)
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab)))
    (unwind-protect
        (progn
          (fset 'hym/workspace-spawn-tab (lambda (_ws _name setup) (funcall setup)))
          (makunbound 'ghostel-environment)
          (fset 'ghostel (lambda (&optional _fresh)
                           (setq captured-dir default-directory
                                 captured-env ghostel-environment
                                 captured-name ghostel-buffer-name)))
          (fset 'ghostel-send-string (lambda (s) (setq sent s)))
          (hym/workspace--start-agent
           '(:name "w" :slug "s" :type worktree :root "/tmp/w")
           "claude" "claude" "sess-1" "it's big")
          (should (equal captured-dir "/tmp/w"))
          (should (equal captured-name "*w: claude*"))
          (should (member "HYM_WORKSPACE_SLUG=s" captured-env))
          (should (member "HYM_WORKSPACE_AGENT=claude" captured-env))
          (should (member "HYM_WORKSPACE_AGENT_SESSION=sess-1" captured-env))
          (should (equal sent "claude 'it'\\''s big'\n")))
      (fset 'hym/workspace-spawn-tab orig-spawn)
      (fmakunbound 'ghostel)
      (fmakunbound 'ghostel-send-string))))

(ert-deftest hym/workspace-run-shell-names-buffer-after-workspace ()
  (let ((captured-name nil)
        (captured-dir nil)
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab))
        (orig-current (symbol-function 'hym/workspace-current)))
    (unwind-protect
        (progn
          (fset 'hym/workspace-spawn-tab (lambda (_ws _name setup) (funcall setup)))
          (fset 'hym/workspace-current
                (lambda () '(:name "tf flows" :slug "tf_flows" :type worktree :root "/tmp/w")))
          (fset 'ghostel (lambda (&optional _fresh)
                           (setq captured-dir default-directory
                                 captured-name ghostel-buffer-name)))
          (hym/workspace-run-shell)
          (should (equal captured-dir "/tmp/w"))
          (should (equal captured-name "*tf flows: shell*")))
      (fset 'hym/workspace-spawn-tab orig-spawn)
      (fset 'hym/workspace-current orig-current)
      (fmakunbound 'ghostel))))

(ert-deftest hym/workspace-new-from-preset-starts-agent-on-success ()
  (let* ((tmp (make-temp-file "hym-preset" t))
         (ws (list :name "fix it" :slug "fix_it" :type 'worktree
                   :root tmp :repos '("web-client") :base-branch "main"))
         (started nil)
         (provision-ok t)
         (err-shown nil)
         (hym/workspace--loaded t)
         (hym/workspace--registry nil)
         (orig-reg (symbol-function 'hym/workspace--register-worktree))
         (orig-open (symbol-function 'hym/workspace-open))
         (orig-prov (symbol-function 'hym/workspace--provision))
         (orig-start (symbol-function 'hym/workspace--start-agent))
         (orig-err (symbol-function 'hym/workspace--show-setup-error)))
    (unwind-protect
        (progn
          (fset 'hym/workspace--register-worktree (lambda (&rest _) ws))
          (fset 'hym/workspace-open (lambda (&rest _) ws))
          (fset 'hym/workspace--provision
                (lambda (_ws _repos _reuse cb) (funcall cb provision-ok)))
          (fset 'hym/workspace--start-agent
                (lambda (_ws name command &optional _session prompt)
                  (setq started (list name command prompt))))
          (fset 'hym/workspace--show-setup-error
                (lambda (&rest _) (setq err-shown t)))
          (let ((hym/workspace-agents '(("claude" . "claude"))))
            (hym/workspace-new-from-preset
             '(:name "frontend" :repos ("web-client") :agent "claude")
             "make the button smaller"))
          (should (equal started '("claude" "claude" "make the button smaller")))
          (should (null err-shown))

          (setq started nil provision-ok nil)
          (let ((hym/workspace-agents '(("claude" . "claude"))))
            (hym/workspace-new-from-preset
             '(:name "frontend" :repos ("web-client") :agent "claude")
             "make the button smaller"))
          (should (null started))
          (should err-shown))
      (fset 'hym/workspace--register-worktree orig-reg)
      (fset 'hym/workspace-open orig-open)
      (fset 'hym/workspace--provision orig-prov)
      (fset 'hym/workspace--start-agent orig-start)
      (fset 'hym/workspace--show-setup-error orig-err)
      (delete-directory tmp t))))

(ert-deftest hym/workspace-notification-text-prefixes-workspace ()
  (with-temp-buffer
    (setq hym/workspace--terminal-workspace "tf flows")
    (should (equal (hym/workspace--notification-text "Claude Code" "Done")
                   "[tf flows] Claude Code: Done"))
    (should (equal (hym/workspace--notification-text "" "Done")
                   "[tf flows] Done")))
  (with-temp-buffer
    (rename-buffer "*ghostel*" t)
    (should (equal (hym/workspace--notification-text "Codex" "Done")
                   "Codex: Done"))
    (should (equal (hym/workspace--notification-text nil "Done")
                   "*ghostel*: Done"))))

(ert-deftest hym/workspace-tag-terminal-records-workspace-name ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'hym/workspace-current)
               (lambda () '(:name "tf flows" :slug "tf_flows"))))
      (hym/workspace--tag-terminal))
    (should (equal hym/workspace--terminal-workspace "tf flows")))
  (with-temp-buffer
    (cl-letf (((symbol-function 'hym/workspace-current) #'ignore))
      (hym/workspace--tag-terminal))
    (should (null hym/workspace--terminal-workspace))))

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
        (other-api-buf (generate-new-buffer " *other-api-evict-test*"))
        (other-web-buf (generate-new-buffer " *other-web-evict-test*"))
        (orig-conductor (symbol-function 'hym/workspace--repo-conductor))
        (orig-kill (symbol-function 'hym/workspace--kill-server))
        (orig-spawn (symbol-function 'hym/workspace-spawn-tab)))
    (unwind-protect
        (let ((other-api (start-process "other-api" other-api-buf "sleep" "30"))
              (other-web (start-process "other-web" other-web-buf "sleep" "30")))
          (set-process-query-on-exit-flag other-api nil)
          (set-process-query-on-exit-flag other-web nil)
          (puthash '("beta" "api") (buffer-name other-api-buf) hym/workspace--servers)
          (puthash '("beta" "web") (buffer-name other-web-buf) hym/workspace--servers)
          (fset 'hym/workspace--repo-conductor
                (lambda (_dir) '((run . "npm run dev"))))
          (fset 'hym/workspace--kill-server
                (lambda (key repo &optional _defer-refresh after-kill)
                  (push (list 'kill key repo) events)
                  (funcall after-kill)))
          (fset 'hym/workspace-spawn-tab
                (lambda (_ws name _setup &optional _reuse)
                  (push (list 'spawn name) events)))
          (hym/workspace--start-server ws "api")
          (should (equal (nreverse events)
                         '((kill "beta" "api") (spawn "server:api"))))
          (delete-process other-api)
          (delete-process other-web))
      (fset 'hym/workspace--repo-conductor orig-conductor)
      (fset 'hym/workspace--kill-server orig-kill)
      (fset 'hym/workspace-spawn-tab orig-spawn)
      (when (buffer-live-p other-api-buf) (kill-buffer other-api-buf))
      (when (buffer-live-p other-web-buf) (kill-buffer other-web-buf)))))

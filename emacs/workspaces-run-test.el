;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-run.el" (file-name-directory load-file-name)))

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
    (hym-workspace-agent-signal "s" "claude" "one" "PermissionRequest")
    (should (eq 'permission
                (plist-get (gethash '("s" "one") hym-workspace--agent-state)
                           :state)))
    (hym-workspace-agent-signal "s" "claude" "one" "Notification")
    (should (eq 'permission
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

(ert-deftest hym-workspace-server-badge-tracks-live-process ()
  (let ((hym-workspace--servers (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~"))
        (buf (generate-new-buffer " *srv-test*")))
    (unwind-protect
        (progn
          (should (null (hym-workspace--server-badge ws)))
          (let ((proc (start-process "srv" buf "sleep" "30")))
            (puthash "s" (buffer-name buf) hym-workspace--servers)
            (should (string-match-p "running" (car (hym-workspace--server-badge ws))))
            (delete-process proc))
          (should (null (hym-workspace--server-badge ws)))
          (should (null (gethash "s" hym-workspace--servers))))
      (kill-buffer buf))))

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

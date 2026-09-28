;; -*- lexical-binding: t -*-

(require 'ert)
(defmacro hym/leader-def (&rest _))
(defmacro general-define-key (&rest _))
(load-file (expand-file-name "git-delta.el" (file-name-directory load-file-name)))
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
                 " claude 'fix it'")))

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
          (should (equal sent " claude 'it'\\''s big'\n")))
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

(defun hym/workspace-run-test--git (dir &rest args)
  "Run git ARGS in DIR, returning trimmed stdout."
  (let ((default-directory dir))
    (with-temp-buffer
      (apply #'call-process "git" nil (list t nil) nil
             "-c" "user.name=t" "-c" "user.email=t@t" args)
      (string-trim (buffer-string)))))

(defun hym/workspace-run-test--repo-behind-origin (dir)
  "Make DIR a repo whose local main lags origin/main, with HEAD branched off origin/main.
Return the origin/main SHA."
  (make-directory dir t)
  (hym/workspace-run-test--git dir "init" "--quiet" "-b" "main")
  (hym/workspace-run-test--git dir "commit" "--allow-empty" "-m" "stale main")
  (hym/workspace-run-test--git dir "checkout" "--quiet" "-b" "upstream")
  (hym/workspace-run-test--git dir "commit" "--allow-empty" "-m" "upstream")
  (let ((upstream (hym/workspace-run-test--git dir "rev-parse" "HEAD")))
    (hym/workspace-run-test--git dir "update-ref" "refs/remotes/origin/main" upstream)
    (hym/workspace-run-test--git dir "checkout" "--quiet" "-b" "feature")
    (hym/workspace-run-test--git dir "branch" "--quiet" "-D" "upstream")
    upstream))

(ert-deftest hym/workspace-review-prompt-uses-origin-merge-base-and-skips-unchanged ()
  (let ((root (make-temp-file "hym-review" t)))
    (unwind-protect
        (let* ((changed-dir (expand-file-name "api" root))
               (clean-dir (expand-file-name "web" root))
               (upstream (hym/workspace-run-test--repo-behind-origin changed-dir))
               (stale (hym/workspace-run-test--git changed-dir "rev-parse" "main")))
          (hym/workspace-run-test--git changed-dir "commit" "--allow-empty" "-m" "mine")
          (hym/workspace-run-test--repo-behind-origin clean-dir)
          (let ((prompt (hym/workspace--review-prompt
                         (list :name "w" :root root :repos '("api" "web")
                               :base-branch "main"))))
            (should (string-match-p (regexp-quote (format "`git diff %s`" upstream))
                                    prompt))
            (should-not (string-match-p stale prompt))
            (should (string-match-p
                     (regexp-quote (format "do not review them:\n- %S" clean-dir))
                     prompt))
            (should-not (string-match-p "CLAUDE.md" prompt))))
      (delete-directory root t))))

(ert-deftest hym/workspace-review-prompt-errors-without-changes ()
  (let ((root (make-temp-file "hym-review" t)))
    (unwind-protect
        (progn
          (hym/workspace-run-test--repo-behind-origin (expand-file-name "api" root))
          (should-error (hym/workspace--review-prompt
                         (list :name "w" :root root :repos '("api")
                               :base-branch "main"))
                        :type 'user-error))
      (delete-directory root t))))

(ert-deftest hym/workspace-review-prompt-includes-uncommitted-only-repos ()
  (let ((root (make-temp-file "hym-review" t)))
    (unwind-protect
        (let ((dir (expand-file-name "api" root)))
          (hym/workspace-run-test--repo-behind-origin dir)
          (with-temp-file (expand-file-name "new.txt" dir) (insert "x"))
          (should (string-match-p
                   (regexp-quote (format "- %S: `git diff" dir))
                   (hym/workspace--review-prompt
                    (list :name "w" :root root :repos '("api")
                          :base-branch "main")))))
      (delete-directory root t))))

(ert-deftest hym/workspace-review-prompt-reports-unresolvable-base ()
  (let ((root (make-temp-file "hym-review" t)))
    (unwind-protect
        (let ((dir (expand-file-name "api" root)))
          (hym/workspace-run-test--repo-behind-origin dir)
          (should (string-match-p
                   "base branch \"develop\" could not be resolved"
                   (hym/workspace--review-prompt
                    (list :name "w" :root root :repos '("api")
                          :base-branch "develop")))))
      (delete-directory root t))))

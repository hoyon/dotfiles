;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-run.el" (file-name-directory load-file-name)))

(ert-deftest hym-workspace-agent-signal-maps-events ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal)))
    (hym-workspace-agent-signal "s" "UserPromptSubmit")
    (should (eq 'working (gethash "s" hym-workspace--agent-state)))
    (hym-workspace-agent-signal "s" "Stop")
    (should (eq 'waiting (gethash "s" hym-workspace--agent-state)))
    (hym-workspace-agent-signal "s" "PermissionRequest")
    (should (eq 'permission (gethash "s" hym-workspace--agent-state)))
    (hym-workspace-agent-signal "s" "Notification")
    (should (eq 'permission (gethash "s" hym-workspace--agent-state)))
    (hym-workspace-agent-signal "s" "SessionEnd")
    (should (null (gethash "s" hym-workspace--agent-state)))))

(ert-deftest hym-workspace-agent-signal-refreshes-only-on-change ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (n 0)
        (orig (symbol-function 'hym-workspace--run-refresh)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--run-refresh (lambda () (setq n (1+ n))))
          (hym-workspace-agent-signal "s" "Stop")
          (hym-workspace-agent-signal "s" "Stop")
          (hym-workspace-agent-signal "s" "PostToolUse")
          (hym-workspace-agent-signal "s" "PreToolUse")
          (should (= n 2)))
      (fset 'hym-workspace--run-refresh orig))))

(ert-deftest hym-workspace-agent-badge-reflects-state ()
  (let ((hym-workspace--agent-state (make-hash-table :test 'equal))
        (ws '(:name "w" :slug "s" :type worktree :root "~")))
    (should (null (hym-workspace--agent-badge ws)))
    (puthash "s" 'working hym-workspace--agent-state)
    (should (null (hym-workspace--agent-badge ws)))
    (puthash "s" 'waiting hym-workspace--agent-state)
    (should (string-match-p "waiting" (car (hym-workspace--agent-badge ws))))
    (puthash "s" 'permission hym-workspace--agent-state)
    (should (string-match-p "permission" (car (hym-workspace--agent-badge ws))))))

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
                                       "claude")))
    (should (member "HYM_WORKSPACE_SLUG=auth" env))
    (should (member "HYM_WORKSPACE_AGENT=claude" env))))

(ert-deftest hym-workspace-agent-env-uses-key-for-slugless-workspace ()
  (let ((env (hym-workspace--agent-env '(:name "Dot Files" :type project :root "~")
                                       "claude")))
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
          (should (member "HYM_WORKSPACE_AGENT=agent-shell" captured-env)))
      (if orig-agent-shell
          (fset 'agent-shell-new-shell orig-agent-shell)
        (fmakunbound 'agent-shell-new-shell))
      (fset 'hym-workspace-current orig-current)
      (fset 'hym-workspace-spawn-tab orig-spawn))))

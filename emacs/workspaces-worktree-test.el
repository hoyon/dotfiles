;; -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))

(ert-deftest hym-workspace-presets-reads-file ()
  (let ((f (make-temp-file "hym-presets")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((:name \"frontend\" :repos (\"web-client\") :agent \"claude\"))"))
          (let ((hym-workspace-presets-file f))
            (let ((ps (hym-workspace-presets)))
              (should (= 1 (length ps)))
              (should (equal (hym-workspace-preset-name (car ps)) "frontend"))
              (should (equal (hym-workspace-preset-repos (car ps)) '("web-client")))
              (should (equal (hym-workspace-preset-base-branch (car ps)) "main"))
              (should (equal (hym-workspace-preset-agent (car ps)) "claude")))))
      (delete-file f))))

(ert-deftest hym-workspace-presets-missing-file-is-nil ()
  (let ((hym-workspace-presets-file "/nonexistent/hym-presets.eld"))
    (should (null (hym-workspace-presets)))))

(ert-deftest hym-workspace-slugify-sanitises ()
  (should (equal (hym-workspace--slugify "Auth Refactor") "auth_refactor"))
  (should (equal (hym-workspace--slugify "  foo-bar!  ") "foo_bar"))
  (should (equal (hym-workspace--slugify "already_ok") "already_ok")))

(ert-deftest hym-workspace-repo-conductor-reads-scripts ()
  (let ((dir (make-temp-file "hym-repo" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "conductor.json" dir)
            (insert "{\"scripts\":{\"setup\":\"echo s\",\"run\":\"echo r\",\"archive\":\"echo a\"}}"))
          (let ((scripts (hym-workspace--repo-conductor dir)))
            (should (equal (alist-get 'setup scripts) "echo s"))
            (should (equal (alist-get 'archive scripts) "echo a")))
          (should (null (hym-workspace--repo-conductor (make-temp-file "empty" t)))))
      (delete-directory dir t))))

(ert-deftest hym-workspace-available-repos-lists-git-repos-conductor-optional ()
  (let ((root (make-temp-file "hym-code" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "with-c/.git" root) t)
          (with-temp-file (expand-file-name "with-c/conductor.json" root) (insert "{}"))
          (make-directory (expand-file-name "no-c/.git" root) t)
          (make-directory (expand-file-name "not-a-repo" root))
          (let ((hym-workspace-code-root root))
            (should (equal (sort (hym-workspace--available-repos) #'string<)
                           '("no-c" "with-c")))))
      (delete-directory root t))))

(defun hym-workspace-worktree-test--init-repo (dir &optional remote)
  "Make DIR a git repo, wired to a dummy origin unless REMOTE is nil."
  (let ((default-directory dir))
    (call-process "git" nil nil nil "init" "--quiet")
    (when remote
      (call-process "git" nil nil nil "remote" "add" "origin"
                    "https://example.invalid/repo.git"))))

(defmacro hym-workspace-worktree-test-with-code (&rest body)
  "Run BODY with a temp code-root containing a `api-server' repo."
  (declare (indent 0) (debug t))
  `(let ((hym-workspace-code-root (make-temp-file "hym-code" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "api-server" hym-workspace-code-root))
           (hym-workspace-worktree-test--init-repo
            (expand-file-name "api-server" hym-workspace-code-root) t)
           (with-temp-file (expand-file-name "api-server/conductor.json"
                                             hym-workspace-code-root)
             (insert "{\"scripts\":{\"setup\":\"mix setup\",\"archive\":\"drop it\"}}"))
           ,@body)
       (delete-directory hym-workspace-code-root t))))

(ert-deftest hym-workspace-provision-command-new-branch ()
  (hym-workspace-worktree-test-with-code
    (let ((ws '(:name "auth" :slug "auth" :type worktree
                :root "~/workspaces/auth" :repos ("api-server")
                :base-branch "main")))
      (let ((cmd (hym-workspace--provision-command ws "api-server" nil)))
        (should (string-match-p "fetch origin" cmd))
        (should (string-match-p
                 (regexp-quote (shell-quote-argument
                                "refs/heads/main:refs/remotes/origin/main"))
                 cmd))
        (should (string-match-p "worktree add -b auth " cmd))
        (should (string-match-p "origin/main" cmd))
        (should (string-match-p "CONDUCTOR_WORKSPACE_NAME=auth" cmd))
        (should (string-match-p "CONDUCTOR_ROOT_PATH=" cmd))
        (should (string-match-p (regexp-quote (shell-quote-argument "mix setup")) cmd))))))

(ert-deftest hym-workspace-provision-command-without-remote-skips-fetch ()
  (hym-workspace-worktree-test-with-code
    (let ((repo (expand-file-name "api-server" hym-workspace-code-root))
          (ws '(:name "auth" :slug "auth" :type worktree
                :root "~/workspaces/auth" :repos ("api-server")
                :base-branch "main")))
      (call-process "git" nil nil nil "-C" repo "remote" "remove" "origin")
      (let ((default-directory repo))
        (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "init")
        (call-process "git" nil nil nil "branch" "-M" "main"))
      (let ((cmd (hym-workspace--worktree-command ws "api-server" nil)))
        (should-not (string-match-p "fetch origin" cmd))
        (should-not (string-match-p "origin/main" cmd))
        (should (string-match-p "worktree add -b auth .* main\\'" cmd))))))

(ert-deftest hym-workspace-provision-command-without-remote-or-base-uses-head ()
  (hym-workspace-worktree-test-with-code
    (let ((repo (expand-file-name "api-server" hym-workspace-code-root))
          (ws '(:name "auth" :slug "auth" :type worktree
                :root "~/workspaces/auth" :repos ("api-server")
                :base-branch "main")))
      (call-process "git" nil nil nil "-C" repo "remote" "remove" "origin")
      (let ((default-directory repo))
        (call-process "git" nil nil nil "commit" "--allow-empty" "-m" "init")
        (call-process "git" nil nil nil "branch" "-M" "trunk"))
      (let ((cmd (hym-workspace--worktree-command ws "api-server" nil)))
        (should-not (string-match-p "fetch origin" cmd))
        (should (string-match-p "worktree add -b auth .* HEAD\\'" cmd))))))

(ert-deftest hym-workspace-provision-command-reuse-branch ()
  (hym-workspace-worktree-test-with-code
    (let ((ws '(:name "auth" :slug "auth" :type worktree
                :root "~/workspaces/auth" :repos ("api-server")
                :base-branch "main")))
      (let ((cmd (hym-workspace--provision-command ws "api-server" t)))
        (should (string-match-p "worktree add auth\\| auth \\|/auth " cmd))
        (should-not (string-match-p "worktree add -b" cmd))
        (should-not (string-match-p "fetch origin" cmd))))))

(ert-deftest hym-workspace-provision-runs-worktree-and-setup-on-success ()
  (hym-workspace-worktree-test-with-code
    (let* ((ran nil)
           (done nil)
           (hym-workspace--provisioning (make-hash-table :test 'equal))
           (hym-workspace--run-async
            (lambda (_name _cmd _buf cb) (push _cmd ran) (funcall cb t)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/workspaces/auth"
                 :repos ("api-server") :base-branch "main")))
      (hym-workspace--provision ws '("api-server") nil (lambda (ok) (setq done ok)))
      (should (eq done t))
      (should (= 2 (length ran)))
      (should (null (gethash "auth" hym-workspace--provisioning))))))

(ert-deftest hym-workspace-provision-adds-all-worktrees-before-any-setup ()
  (hym-workspace-worktree-test-with-code
    (make-directory (expand-file-name "web-client" hym-workspace-code-root))
    (with-temp-file (expand-file-name "web-client/conductor.json"
                                      hym-workspace-code-root)
      (insert "{\"scripts\":{\"setup\":\"npm install\"}}"))
    (let* ((ran nil)
           (done nil)
           (hym-workspace--provisioning (make-hash-table :test 'equal))
           (hym-workspace--run-async
            (lambda (_name command _buffer callback)
              (setq ran (append ran (list command)))
              (funcall callback t)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/workspaces/auth"
                 :repos ("api-server" "web-client") :base-branch "main")))
      (hym-workspace--provision
       ws '("api-server" "web-client") nil (lambda (ok) (setq done ok)))
      (should (eq done t))
      (should (= 4 (length ran)))
      (should (seq-every-p (lambda (command)
                             (string-match-p "worktree add" command))
                           (seq-take ran 2)))
      (should (seq-every-p (lambda (command)
                             (not (string-match-p "worktree add" command)))
                           (seq-drop ran 2)))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "mix setup")) (nth 2 ran)))
      (should (string-match-p
               (regexp-quote (shell-quote-argument "npm install")) (nth 3 ran))))))

(ert-deftest hym-workspace-provision-stops-and-marks-failed ()
  (hym-workspace-worktree-test-with-code
    (let* ((done 'unset)
           (hym-workspace--provisioning (make-hash-table :test 'equal))
           (hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb nil)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/workspaces/auth"
                 :repos ("api-server") :base-branch "main")))
      (hym-workspace--provision ws '("api-server") nil (lambda (ok) (setq done ok)))
      (should (eq done nil))
      (should (eq 'failed (plist-get (gethash "auth" hym-workspace--provisioning) :state))))))

(defun hym-workspace-worktree-test--make-assets (root repo &rest specs)
  "Create `.claude' entries under ROOT/REPO from SPECS of (KIND . NAME)."
  (dolist (spec specs)
    (let ((dir (expand-file-name (format "%s/.claude/%s" repo (car spec)) root)))
      (make-directory dir t)
      (if (equal (car spec) "agents")
          (with-temp-file (expand-file-name (cdr spec) dir) (insert "agent"))
        (make-directory (expand-file-name (cdr spec) dir) t)))))

(ert-deftest hym-workspace-sync-claude-assets-links-skills-and-agents ()
  (let ((root (make-temp-file "hym-ws-root" t)))
    (unwind-protect
        (let ((ws (list :name "auth" :slug "auth" :type 'worktree :root root
                        :repos '("api-server") :base-branch "main")))
          (hym-workspace-worktree-test--make-assets
           root "api-server" '("skills" . "security-reviewer") '("agents" . "writer.md"))
          (hym-workspace--sync-claude-assets ws '("api-server"))
          (let ((skill (expand-file-name ".claude/skills/security-reviewer" root))
                (agent (expand-file-name ".claude/agents/writer.md" root)))
            (should (file-symlink-p skill))
            (should (file-directory-p skill))
            (should (file-symlink-p agent))
            (should (file-exists-p agent))))
      (delete-directory root t))))

(ert-deftest hym-workspace-sync-claude-assets-first-repo-wins-collisions ()
  (let ((root (make-temp-file "hym-ws-root" t)))
    (unwind-protect
        (let ((ws (list :name "auth" :slug "auth" :type 'worktree :root root
                        :repos '("a" "b") :base-branch "main")))
          (hym-workspace-worktree-test--make-assets root "a" '("skills" . "shared"))
          (hym-workspace-worktree-test--make-assets root "b" '("skills" . "shared"))
          (hym-workspace--sync-claude-assets ws '("a" "b"))
          (should (equal (expand-file-name "a/.claude/skills/shared" root)
                         (file-symlink-p
                          (expand-file-name ".claude/skills/shared" root)))))
      (delete-directory root t))))

(ert-deftest hym-workspace-sync-claude-assets-prunes-dangling-links ()
  (let ((root (make-temp-file "hym-ws-root" t)))
    (unwind-protect
        (let ((ws (list :name "auth" :slug "auth" :type 'worktree :root root
                        :repos '("gone") :base-branch "main")))
          (hym-workspace-worktree-test--make-assets root "gone" '("skills" . "old"))
          (hym-workspace--sync-claude-assets ws '("gone"))
          (delete-directory (expand-file-name "gone" root) t)
          (hym-workspace--sync-claude-assets ws nil)
          (should-not (file-symlink-p (expand-file-name ".claude/skills/old" root))))
      (delete-directory root t))))

(ert-deftest hym-workspace-sync-claude-assets-noop-without-claude-dir ()
  (let ((root (make-temp-file "hym-ws-root" t)))
    (unwind-protect
        (let ((ws (list :name "auth" :slug "auth" :type 'worktree :root root
                        :repos '("bare") :base-branch "main")))
          (make-directory (expand-file-name "bare" root))
          (hym-workspace--sync-claude-assets ws '("bare"))
          (should-not (file-exists-p (expand-file-name ".claude" root))))
      (delete-directory root t))))

(ert-deftest hym-workspace-provision-links-claude-assets-before-setup ()
  (hym-workspace-worktree-test-with-code
    (let* ((root (make-temp-file "hym-ws-root" t))
           (linked nil)
           (hym-workspace--provisioning (make-hash-table :test 'equal))
           (hym-workspace--run-async
            (lambda (_name command _buffer callback)
              (when (string-match-p "worktree add" command)
                (hym-workspace-worktree-test--make-assets
                 root "api-server" '("skills" . "audit-tests")))
              (unless (string-match-p "worktree add" command)
                (setq linked (file-symlink-p
                              (expand-file-name ".claude/skills/audit-tests" root))))
              (funcall callback t)))
           (ws (list :name "auth" :slug "auth" :type 'worktree :root root
                     :repos '("api-server") :base-branch "main")))
      (unwind-protect
          (progn
            (hym-workspace--provision ws '("api-server") nil #'ignore)
            (should linked))
        (delete-directory root t)))))

(ert-deftest hym-workspace-provisioning-badge-reflects-state ()
  (let ((hym-workspace--provisioning (make-hash-table :test 'equal))
        (ws '(:name "auth" :slug "auth" :type worktree :root "~")))
    (should (null (hym-workspace--provisioning-badge ws)))
    (puthash "auth" '(:repo "api-server" :state running) hym-workspace--provisioning)
    (should (string-match-p "provisioning api-server"
                            (car (hym-workspace--provisioning-badge ws))))
    (puthash "auth" '(:repo "api-server" :state failed) hym-workspace--provisioning)
    (should (string-match-p "failed"
                            (car (hym-workspace--provisioning-badge ws))))))

(ert-deftest hym-workspace-read-repos-accumulates-until-done ()
  (let ((picks (list "api-server" "web-client" "[done]"))
        (native-comp-enable-subr-trampolines nil)
        (orig-cr (symbol-function 'completing-read))
        (orig-ar (symbol-function 'hym-workspace--available-repos)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--available-repos
                (lambda () '("api-server" "web-client" "admin-portal")))
          (fset 'completing-read (lambda (&rest _) (pop picks)))
          (should (equal (hym-workspace--read-repos)
                         '("api-server" "web-client"))))
      (fset 'completing-read orig-cr)
      (fset 'hym-workspace--available-repos orig-ar))))

(defmacro hym-workspace-worktree-test-with-registry (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((temp (make-temp-file "hym-ws" nil ".eld"))
          (hym-workspace-registry-file temp)
          (hym-workspace--registry nil)
          (hym-workspace--loaded t)
          (hym-workspace-home (make-temp-file "hym-home" t))
          (hym-workspace--provisioning (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory hym-workspace-home t)
       (when (file-exists-p temp) (delete-file temp)))))

(defvar hym-workspace-worktree-test--events nil)

(ert-deftest hym-workspace-register-worktree-validates-and-stores ()
  (hym-workspace-worktree-test-with-registry
    (let ((ws (hym-workspace--register-worktree "Auth Refactor" "main"
                                                '("api-server"))))
      (should (equal (hym-workspace-slug ws) "auth_refactor"))
      (should (equal (hym-workspace-name ws) "Auth Refactor"))
      (should (equal (hym-workspace-repos ws) '("api-server")))
      (should-error (hym-workspace--register-worktree "auth refactor" "main"
                                                      '("api-server")))
      (should-error (hym-workspace--register-worktree "no repos" "main" nil)))))

(ert-deftest hym-workspace-add-repo-appends-on-success ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (hym-workspace-add-repo ws "web-client")
      (should (equal (hym-workspace-repos (hym-workspace-get "auth"))
                     '("api-server" "web-client"))))))

(ert-deftest hym-workspace-add-repo-links-claude-assets-keeping-existing ()
  (hym-workspace-worktree-test-with-registry
    (let* ((ws (hym-workspace--register-worktree "auth" "main" '("api-server")))
           (root (hym-workspace-root ws))
           (hym-workspace--run-async
            (lambda (_n command _b cb)
              (when (string-match-p "worktree add" command)
                (hym-workspace-worktree-test--make-assets
                 root "web-client" '("skills" . "formatter") '("skills" . "shared")))
              (funcall cb t))))
      (hym-workspace-worktree-test--make-assets
       root "api-server" '("skills" . "audit-tests") '("skills" . "shared"))
      (hym-workspace--sync-claude-assets ws '("api-server"))
      (hym-workspace-add-repo ws "web-client")
      (should (file-symlink-p (expand-file-name ".claude/skills/formatter" root)))
      (should (file-symlink-p (expand-file-name ".claude/skills/audit-tests" root)))
      (should (equal (expand-file-name "api-server/.claude/skills/shared" root)
                     (file-symlink-p
                      (expand-file-name ".claude/skills/shared" root)))))))

(ert-deftest hym-workspace-add-repo-rejects-duplicate ()
  (hym-workspace-worktree-test-with-registry
    (let ((ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (should-error (hym-workspace-add-repo ws "api-server")))))

(ert-deftest hym-workspace-archive-command-removes-worktree ()
  (let ((ws '(:name "auth" :slug "auth" :type worktree
              :root "~/workspaces/auth" :repos ("api-server")
              :base-branch "main"))
        (hym-workspace-code-root "~/code"))
    (let ((cmd (hym-workspace--archive-command ws "api-server")))
      (should (string-match-p "worktree remove" cmd)))))

(ert-deftest hym-workspace-unarchive-clears-flag ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (hym-workspace-put (plist-put (copy-sequence (hym-workspace-get "auth"))
                                    :archived t))
      (hym-workspace-unarchive (hym-workspace-get "auth"))
      (should-not (hym-workspace-archived-p (hym-workspace-get "auth"))))))

(ert-deftest hym-workspace-register-worktree-rejects-empty-slug ()
  (hym-workspace-worktree-test-with-registry
    (should-error (hym-workspace--register-worktree "!!!" "main" '("api-server")))))

(ert-deftest hym-workspace-archive-worktree-marks-archived-on-success ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (make-directory (expand-file-name "api-server/.git" (hym-workspace-root ws)) t)
      (cl-letf (((symbol-function 'hym-workspace--repo-worktree-registered-p)
                 (lambda (_ws _repo) t)))
        (hym-workspace-archive-worktree ws))
      (should (hym-workspace-archived-p (hym-workspace-get "auth")))
      (should (null (gethash "auth" hym-workspace--provisioning))))))

(ert-deftest hym-workspace-archive-worktree-skips-already-removed-repos ()
  (hym-workspace-worktree-test-with-registry
    (let* ((ran nil)
           (hym-workspace--run-async
            (lambda (_n cmd _b cb) (push cmd ran) (funcall cb t)))
           (ws (hym-workspace--register-worktree "auth" "main" '("gone" "left"))))
      (make-directory (expand-file-name "left/.git" (hym-workspace-root ws)) t)
      (cl-letf (((symbol-function 'hym-workspace--repo-worktree-registered-p)
                 (lambda (_ws repo) (equal repo "left"))))
        (hym-workspace-archive-worktree ws))
      (should (= 1 (length ran)))
      (should-not (string-match-p "/gone" (car ran)))
      (should (hym-workspace-archived-p (hym-workspace-get "auth")))
      (should (null (gethash "auth" hym-workspace--provisioning))))))

(ert-deftest hym-workspace-archive-worktree-leaves-active-on-failure ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb nil)))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (make-directory (expand-file-name "api-server/.git" (hym-workspace-root ws)) t)
      (cl-letf (((symbol-function 'hym-workspace--repo-worktree-registered-p)
                 (lambda (_ws _repo) t)))
        (hym-workspace-archive-worktree ws))
      (should-not (hym-workspace-archived-p (hym-workspace-get "auth")))
      (should (eq 'archive-failed
                  (plist-get (gethash "auth" hym-workspace--provisioning) :state))))))

(ert-deftest hym-workspace-archive-worktree-stops-servers-before-closing ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace-worktree-test--events nil)
          (hym-workspace--run-async
           (lambda (_n _c _b cb)
             (push 'archive hym-workspace-worktree-test--events)
             (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (make-directory (expand-file-name "api-server/.git" (hym-workspace-root ws)) t)
      (cl-letf (((symbol-function 'hym-workspace-kill-workspace-servers)
                 (lambda (_ws &optional _defer-refresh)
                   (push 'kill hym-workspace-worktree-test--events)))
                ((symbol-function 'hym-workspace-close)
                 (lambda (_ws) (push 'close hym-workspace-worktree-test--events)))
                ((symbol-function 'hym-workspace--repo-worktree-registered-p)
                 (lambda (_ws _repo) t)))
        (hym-workspace-archive-worktree ws))
      (should (equal '(archive close kill)
                     hym-workspace-worktree-test--events)))))

(ert-deftest hym-workspace-name-from-prompt-dedupes ()
  (let ((hym-workspace--loaded t)
        (hym-workspace--registry
         (list '(:name "make the button" :slug "make_the_button"))))
    (should (equal (hym-workspace--name-from-prompt
                    "Fix the login flow please now urgently")
                   "fix the login flow please"))
    (should (equal (hym-workspace--name-from-prompt "Make the button")
                   "make the button 2"))))

(ert-deftest hym-workspace-name-from-prompt-dedupes-on-slug-collision ()
  (let ((hym-workspace--loaded t)
        (hym-workspace--registry
         (list '(:name "Auth Service!" :slug "auth_service"))))
    (should (equal (hym-workspace--name-from-prompt "Auth service")
                   "auth service 2"))))

(ert-deftest hym-workspace-name-from-prompt-handles-empty ()
  (let ((hym-workspace--loaded t)
        (hym-workspace--registry nil))
    (should (equal (hym-workspace--name-from-prompt "!!!") "workspace"))))

(ert-deftest hym-workspace-provision-retry-reprovisions-only-missing ()
  (hym-workspace-worktree-test-with-registry
    (let* ((ran nil)
           (hym-workspace--run-async
            (lambda (_n cmd _b cb) (push cmd ran) (funcall cb t)))
           (ws (hym-workspace--register-worktree "w" "main" '("a" "b"))))
      (puthash "w" '(:repo "b" :state failed) hym-workspace--provisioning)
      (make-directory (expand-file-name "a/.git" (hym-workspace-root ws)) t)
      (hym-workspace-provision-retry ws)
      (should (null (gethash "w" hym-workspace--provisioning)))
      (should (= 1 (length ran)))
      (should-not (string-match-p "/a " (car ran))))))

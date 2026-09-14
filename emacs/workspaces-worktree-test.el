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
           (hym-workspace--jobs (make-hash-table :test 'equal))
           (hym-workspace--run-async
            (lambda (_name _cmd _buf cb) (push _cmd ran) (funcall cb t)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/workspaces/auth"
                 :repos ("api-server") :base-branch "main")))
      (hym-workspace--provision ws '("api-server") nil (lambda (ok) (setq done ok)))
      (should (eq done t))
      (should (= 2 (length ran)))
      (should (null (gethash "auth" hym-workspace--jobs))))))

(ert-deftest hym-workspace-provision-adds-all-worktrees-before-any-setup ()
  (hym-workspace-worktree-test-with-code
    (make-directory (expand-file-name "web-client" hym-workspace-code-root))
    (with-temp-file (expand-file-name "web-client/conductor.json"
                                      hym-workspace-code-root)
      (insert "{\"scripts\":{\"setup\":\"npm install\"}}"))
    (let* ((ran nil)
           (done nil)
           (hym-workspace--jobs (make-hash-table :test 'equal))
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
           (hym-workspace--jobs (make-hash-table :test 'equal))
           (hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb nil)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/workspaces/auth"
                 :repos ("api-server") :base-branch "main")))
      (hym-workspace--provision ws '("api-server") nil (lambda (ok) (setq done ok)))
      (should (eq done nil))
      (should (eq 'failed (plist-get (gethash "auth" hym-workspace--jobs) :state))))))

(defun hym-workspace-worktree-test--asset-paths (name)
  "Return representative Claude and Codex asset paths for NAME."
  (list (format ".claude/skills/%s/SKILL.md" name)
        (format ".claude/agents/%s.md" name)
        (format ".agents/skills/%s/SKILL.md" name)
        (format ".codex/agents/%s.toml" name)))

(defun hym-workspace-worktree-test--asset-links (name)
  "Return the root entries linked for assets named NAME."
  (mapcar (lambda (path)
            (if (equal (file-name-nondirectory path) "SKILL.md")
                (directory-file-name (file-name-directory path))
              path))
          (hym-workspace-worktree-test--asset-paths name)))

(defun hym-workspace-worktree-test--make-assets (root repo &rest names)
  "Create Claude and Codex skills and agent files for NAMES under ROOT/REPO."
  (dolist (name names)
    (dolist (path (hym-workspace-worktree-test--asset-paths name))
      (let ((file (expand-file-name path (expand-file-name repo root))))
        (make-directory (file-name-directory file) t)
        (with-temp-file file (insert repo))))))

(defun hym-workspace-worktree-test--should-link-assets (root repo name)
  "Assert all NAME assets at ROOT link to REPO and expose its contents."
  (dolist (path (hym-workspace-worktree-test--asset-links name))
    (ert-info ((format "Asset link: %s" path))
      (should (equal (expand-file-name path (expand-file-name repo root))
                     (file-symlink-p (expand-file-name path root))))))
  (dolist (path (hym-workspace-worktree-test--asset-paths name))
    (ert-info ((format "Asset contents: %s" path))
      (should (equal repo (with-temp-buffer
                            (insert-file-contents (expand-file-name path root))
                            (buffer-string)))))))

(defmacro hym-workspace-worktree-test-with-assets (&rest body)
  "Run BODY with a temporary workspace bound to WS and its directory to ROOT."
  (declare (indent 0) (debug t))
  `(let* ((root (make-temp-file "hym-ws-root" t))
          (ws (list :name "auth" :slug "auth" :type 'worktree :root root
                    :repos '("api-server") :base-branch "main")))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(ert-deftest hym-workspace-sync-assets-links-skills-and-agents ()
  (hym-workspace-worktree-test-with-assets
    (hym-workspace-worktree-test--make-assets root "api-server" "review")
    ;; Repeated refreshes must preserve the same working links.
    (dotimes (_ 2)
      (hym-workspace--sync-claude-assets ws '("api-server"))
      (hym-workspace-worktree-test--should-link-assets root "api-server" "review"))))

(ert-deftest hym-workspace-sync-assets-first-repo-wins-collisions ()
  (hym-workspace-worktree-test-with-assets
    (hym-workspace-worktree-test--make-assets root "a" "shared")
    (hym-workspace-worktree-test--make-assets root "b" "shared")
    (hym-workspace--sync-claude-assets ws '("a" "b"))
    (hym-workspace-worktree-test--should-link-assets root "a" "shared")))

(ert-deftest hym-workspace-sync-assets-prunes-dangling-links ()
  (hym-workspace-worktree-test-with-assets
    (hym-workspace-worktree-test--make-assets root "gone" "old")
    (hym-workspace-worktree-test--make-assets root "kept" "live")
    (hym-workspace--sync-claude-assets ws '("gone" "kept"))
    (delete-directory (expand-file-name "gone" root) t)
    (hym-workspace--sync-claude-assets ws '("kept"))
    (dolist (path (hym-workspace-worktree-test--asset-links "old"))
      (ert-info ((format "Dangling asset: %s" path))
        (should-not (file-symlink-p (expand-file-name path root)))))
    (hym-workspace-worktree-test--should-link-assets root "kept" "live")))

(ert-deftest hym-workspace-sync-assets-noop-without-asset-dirs ()
  (hym-workspace-worktree-test-with-assets
    (make-directory (expand-file-name "bare" root))
    (hym-workspace--sync-claude-assets ws '("bare"))
    (dolist (dir '(".claude" ".agents" ".codex"))
      (should-not (file-exists-p (expand-file-name dir root))))))

(ert-deftest hym-workspace-provision-links-assets-before-setup ()
  (hym-workspace-worktree-test-with-code
    (hym-workspace-worktree-test-with-assets
      (let* ((setup-ran nil)
             (hym-workspace--jobs (make-hash-table :test 'equal))
             (hym-workspace--run-async
              (lambda (_name command _buffer callback)
                (if (string-match-p "worktree add" command)
                    (hym-workspace-worktree-test--make-assets root "api-server" "review")
                  (hym-workspace-worktree-test--should-link-assets root "api-server" "review")
                  (setq setup-ran t))
                (funcall callback t))))
        (hym-workspace--provision ws '("api-server") nil #'ignore)
        (should setup-ran)))))

(ert-deftest hym-workspace-job-badge-reflects-state ()
  (let ((hym-workspace--jobs (make-hash-table :test 'equal))
        (ws '(:name "auth" :slug "auth" :type worktree :root "~")))
    (should (null (hym-workspace--job-badge ws)))
    (puthash "auth" '(:repo "api-server" :state running) hym-workspace--jobs)
    (should (string-match-p "provisioning api-server"
                            (car (hym-workspace--job-badge ws))))
    (puthash "auth" '(:repo "api-server" :state failed) hym-workspace--jobs)
    (should (string-match-p "failed"
                            (car (hym-workspace--job-badge ws))))))

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
          (hym-workspace--jobs (make-hash-table :test 'equal)))
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

(ert-deftest hym-workspace-add-repo-links-assets-keeping-existing ()
  (hym-workspace-worktree-test-with-registry
    (let* ((ws (hym-workspace--register-worktree "auth" "main" '("api-server")))
           (root (hym-workspace-root ws))
           (hym-workspace--run-async
            (lambda (_n command _b cb)
              (when (string-match-p "worktree add" command)
                (hym-workspace-worktree-test--make-assets root "web-client" "formatter" "shared"))
              (funcall cb t))))
      (hym-workspace-worktree-test--make-assets root "api-server" "review" "shared")
      (hym-workspace--sync-claude-assets ws '("api-server"))
      (hym-workspace-add-repo ws "web-client")
      (hym-workspace-worktree-test--should-link-assets root "web-client" "formatter")
      (hym-workspace-worktree-test--should-link-assets root "api-server" "review")
      (hym-workspace-worktree-test--should-link-assets root "api-server" "shared"))))

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
      (should (null (gethash "auth" hym-workspace--jobs))))))

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
      (should (null (gethash "auth" hym-workspace--jobs))))))

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
                  (plist-get (gethash "auth" hym-workspace--jobs) :state))))))

(ert-deftest hym-workspace-archive-worktree-tears-down-before-closing ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace-worktree-test--events nil)
          (hym-workspace--run-async
           (lambda (_n _c _b cb)
             (push 'archive hym-workspace-worktree-test--events)
             (funcall cb t)))
          (hym-workspace-teardown-functions
           (list (lambda (_ws)
                   (push 'kill hym-workspace-worktree-test--events))))
          (ws (hym-workspace--register-worktree "auth" "main" '("api-server"))))
      (make-directory (expand-file-name "api-server/.git" (hym-workspace-root ws)) t)
      (cl-letf (((symbol-function 'hym-workspace-close)
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
      (puthash "w" '(:repo "b" :state failed) hym-workspace--jobs)
      (make-directory (expand-file-name "a/.git" (hym-workspace-root ws)) t)
      (hym-workspace-provision-retry ws)
      (should (null (gethash "w" hym-workspace--jobs)))
      (should (= 1 (length ran)))
      (should-not (string-match-p "/a " (car ran))))))

(ert-deftest hym-workspace-presets-corrupt-file-signals ()
  (let ((f (make-temp-file "hym-presets")))
    (unwind-protect
        (progn
          (with-temp-file f (insert "((:name \"frontend\""))
          (let ((hym-workspace-presets-file f))
            (should-error (hym-workspace-presets))))
      (delete-file f))))

(ert-deftest hym-workspace-setup-command-nil-without-conductor-script ()
  (let* ((root (make-temp-file "hym-code" t))
         (hym-workspace-code-root root)
         (ws '(:name "w" :slug "w" :type worktree :root "/tmp/ws-w"
               :repos ("api-server") :base-branch "main")))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "api-server" root) t)
          (should (null (hym-workspace--setup-command ws "api-server"))))
      (delete-directory root t))))

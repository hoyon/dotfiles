;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-worktree.el" (file-name-directory load-file-name)))

(ert-deftest hym-workspace-presets-reads-file ()
  (let ((f (make-temp-file "hym-presets")))
    (unwind-protect
        (progn
          (with-temp-file f
            (insert "((:name \"frontend\" :repos (\"ploy-client\") :agent \"claude\"))"))
          (let ((hym-workspace-presets-file f))
            (let ((ps (hym-workspace-presets)))
              (should (= 1 (length ps)))
              (should (equal (hym-workspace-preset-name (car ps)) "frontend"))
              (should (equal (hym-workspace-preset-repos (car ps)) '("ploy-client")))
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

(defmacro hym-workspace-worktree-test-with-code (&rest body)
  "Run BODY with a temp code-root containing a `ploy-server' repo."
  (declare (indent 0) (debug t))
  `(let ((hym-workspace-code-root (make-temp-file "hym-code" t)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "ploy-server" hym-workspace-code-root))
           (with-temp-file (expand-file-name "ploy-server/conductor.json"
                                             hym-workspace-code-root)
             (insert "{\"scripts\":{\"setup\":\"mix setup\",\"archive\":\"drop it\"}}"))
           ,@body)
       (delete-directory hym-workspace-code-root t))))

(ert-deftest hym-workspace-provision-command-new-branch ()
  (hym-workspace-worktree-test-with-code
    (let ((ws '(:name "auth" :slug "auth" :type worktree
                :root "~/orca/workspaces/auth" :repos ("ploy-server")
                :base-branch "main")))
      (let ((cmd (hym-workspace--provision-command ws "ploy-server" nil)))
        (should (string-match-p "worktree add -b auth " cmd))
        (should (string-match-p "main" cmd))
        (should (string-match-p "CONDUCTOR_WORKSPACE_NAME=auth" cmd))
        (should (string-match-p "CONDUCTOR_ROOT_PATH=" cmd))
        (should (string-match-p (regexp-quote (shell-quote-argument "mix setup")) cmd))))))

(ert-deftest hym-workspace-provision-command-reuse-branch ()
  (hym-workspace-worktree-test-with-code
    (let ((ws '(:name "auth" :slug "auth" :type worktree
                :root "~/orca/workspaces/auth" :repos ("ploy-server")
                :base-branch "main")))
      (let ((cmd (hym-workspace--provision-command ws "ploy-server" t)))
        (should (string-match-p "worktree add auth\\| auth \\|/auth " cmd))
        (should-not (string-match-p "worktree add -b" cmd))))))

(ert-deftest hym-workspace-provision-runs-worktree-and-setup-on-success ()
  (hym-workspace-worktree-test-with-code
    (let* ((ran nil)
           (done nil)
           (hym-workspace--provisioning (make-hash-table :test 'equal))
           (hym-workspace--run-async
            (lambda (_name _cmd _buf cb) (push _cmd ran) (funcall cb t)))
           (ws '(:name "auth" :slug "auth" :type worktree
                 :root "~/orca/workspaces/auth"
                 :repos ("ploy-server") :base-branch "main")))
      (hym-workspace--provision ws '("ploy-server") nil (lambda (ok) (setq done ok)))
      (should (eq done t))
      (should (= 2 (length ran)))
      (should (null (gethash "auth" hym-workspace--provisioning))))))

(ert-deftest hym-workspace-provision-adds-all-worktrees-before-any-setup ()
  (hym-workspace-worktree-test-with-code
    (make-directory (expand-file-name "ploy-client" hym-workspace-code-root))
    (with-temp-file (expand-file-name "ploy-client/conductor.json"
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
                 :root "~/orca/workspaces/auth"
                 :repos ("ploy-server" "ploy-client") :base-branch "main")))
      (hym-workspace--provision
       ws '("ploy-server" "ploy-client") nil (lambda (ok) (setq done ok)))
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
                 :root "~/orca/workspaces/auth"
                 :repos ("ploy-server") :base-branch "main")))
      (hym-workspace--provision ws '("ploy-server") nil (lambda (ok) (setq done ok)))
      (should (eq done nil))
      (should (eq 'failed (plist-get (gethash "auth" hym-workspace--provisioning) :state))))))

(ert-deftest hym-workspace-provisioning-badge-reflects-state ()
  (let ((hym-workspace--provisioning (make-hash-table :test 'equal))
        (ws '(:name "auth" :slug "auth" :type worktree :root "~")))
    (should (null (hym-workspace--provisioning-badge ws)))
    (puthash "auth" '(:repo "ploy-server" :state running) hym-workspace--provisioning)
    (should (string-match-p "provisioning ploy-server"
                            (car (hym-workspace--provisioning-badge ws))))
    (puthash "auth" '(:repo "ploy-server" :state failed) hym-workspace--provisioning)
    (should (string-match-p "failed"
                            (car (hym-workspace--provisioning-badge ws))))))

(ert-deftest hym-workspace-read-repos-accumulates-until-done ()
  (let ((picks (list "ploy-server" "ploy-client" "[done]"))
        (native-comp-enable-subr-trampolines nil)
        (orig-cr (symbol-function 'completing-read))
        (orig-ar (symbol-function 'hym-workspace--available-repos)))
    (unwind-protect
        (progn
          (fset 'hym-workspace--available-repos
                (lambda () '("ploy-server" "ploy-client" "employee-portal")))
          (fset 'completing-read (lambda (&rest _) (pop picks)))
          (should (equal (hym-workspace--read-repos)
                         '("ploy-server" "ploy-client"))))
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

(ert-deftest hym-workspace-register-worktree-validates-and-stores ()
  (hym-workspace-worktree-test-with-registry
    (let ((ws (hym-workspace--register-worktree "Auth Refactor" "main"
                                                '("ploy-server"))))
      (should (equal (hym-workspace-slug ws) "auth_refactor"))
      (should (equal (hym-workspace-name ws) "Auth Refactor"))
      (should (equal (hym-workspace-repos ws) '("ploy-server")))
      (should-error (hym-workspace--register-worktree "auth refactor" "main"
                                                      '("ploy-server")))
      (should-error (hym-workspace--register-worktree "no repos" "main" nil)))))

(ert-deftest hym-workspace-add-repo-appends-on-success ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("ploy-server"))))
      (hym-workspace-add-repo ws "ploy-client")
      (should (equal (hym-workspace-repos (hym-workspace-get "auth"))
                     '("ploy-server" "ploy-client"))))))

(ert-deftest hym-workspace-add-repo-rejects-duplicate ()
  (hym-workspace-worktree-test-with-registry
    (let ((ws (hym-workspace--register-worktree "auth" "main" '("ploy-server"))))
      (should-error (hym-workspace-add-repo ws "ploy-server")))))

(ert-deftest hym-workspace-archive-command-removes-worktree ()
  (let ((ws '(:name "auth" :slug "auth" :type worktree
              :root "~/orca/workspaces/auth" :repos ("ploy-server")
              :base-branch "main"))
        (hym-workspace-code-root "~/code"))
    (let ((cmd (hym-workspace--archive-command ws "ploy-server")))
      (should (string-match-p "worktree remove" cmd)))))

(ert-deftest hym-workspace-unarchive-clears-flag ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("ploy-server"))))
      (hym-workspace-put (plist-put (copy-sequence (hym-workspace-get "auth"))
                                    :archived t))
      (hym-workspace-unarchive (hym-workspace-get "auth"))
      (should-not (hym-workspace-archived-p (hym-workspace-get "auth"))))))

(ert-deftest hym-workspace-register-worktree-rejects-empty-slug ()
  (hym-workspace-worktree-test-with-registry
    (should-error (hym-workspace--register-worktree "!!!" "main" '("ploy-server")))))

(ert-deftest hym-workspace-archive-worktree-marks-archived-on-success ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb t)))
          (ws (hym-workspace--register-worktree "auth" "main" '("ploy-server"))))
      (hym-workspace-archive-worktree ws)
      (should (hym-workspace-archived-p (hym-workspace-get "auth")))
      (should (null (gethash "auth" hym-workspace--provisioning))))))

(ert-deftest hym-workspace-archive-worktree-leaves-active-on-failure ()
  (hym-workspace-worktree-test-with-registry
    (let ((hym-workspace--run-async (lambda (_n _c _b cb) (funcall cb nil)))
          (ws (hym-workspace--register-worktree "auth" "main" '("ploy-server"))))
      (hym-workspace-archive-worktree ws)
      (should-not (hym-workspace-archived-p (hym-workspace-get "auth")))
      (should (eq 'archive-failed
                  (plist-get (gethash "auth" hym-workspace--provisioning) :state))))))

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

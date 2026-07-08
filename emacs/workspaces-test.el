;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))

(defmacro hym-workspace-test-with-empty-registry (&rest body)
  "Run BODY with an isolated, empty, temp-file-backed registry."
  (declare (indent 0) (debug t))
  `(let* ((hym-workspace--temp (make-temp-file "hym-ws" nil ".eld"))
          (hym-workspace-registry-file hym-workspace--temp)
          (hym-workspace--registry nil)
          (hym-workspace--loaded t)
          (hym-workspace--load-failed nil))
     (unwind-protect (progn ,@body)
       (when (file-exists-p hym-workspace--temp)
         (delete-file hym-workspace--temp)))))

(ert-deftest hym-workspace-put-get-roundtrip ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "zippy" :type worktree :root "~/orca/zippy"
                         :repos ("ploy-server" "ploy-client") :base-branch "main"))
    (should (equal (hym-workspace-name (hym-workspace-get "zippy")) "zippy"))
    (should (equal (hym-workspace-type (hym-workspace-get "zippy")) 'worktree))))

(ert-deftest hym-workspace-put-upsert-preserves-position ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "a" :type project :root "~/a"))
    (hym-workspace-put '(:name "b" :type project :root "~/b"))
    (hym-workspace-put '(:name "a" :type project :root "~/a2"))
    (should (equal (mapcar #'hym-workspace-name (hym-workspace-registry))
                   '("a" "b")))
    (should (equal (hym-workspace-root (hym-workspace-get "a"))
                   (expand-file-name "~/a2")))))

(ert-deftest hym-workspace-save-load-roundtrip ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "notes" :type notes :root "~/org"))
    (let ((hym-workspace--registry nil)
          (hym-workspace--loaded nil))
      (should (equal (hym-workspace-name (car (hym-workspace-registry))) "notes")))))

(ert-deftest hym-workspace-remove-deletes-and-persists ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "keep" :type project :root "~/k"))
    (hym-workspace-put '(:name "drop" :type project :root "~/d"))
    (hym-workspace-remove "drop")
    (should (null (hym-workspace-get "drop")))
    (should (equal (mapcar #'hym-workspace-name (hym-workspace-registry))
                   '("keep")))
    (let ((hym-workspace--registry nil)
          (hym-workspace--loaded nil))
      (should (null (hym-workspace-get "drop")))
      (should (equal (mapcar #'hym-workspace-name (hym-workspace-registry))
                     '("keep"))))))

(ert-deftest hym-workspace-active-excludes-archived ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "live" :type project :root "~/l"))
    (hym-workspace-put '(:name "old" :type project :root "~/o" :archived t))
    (should (equal (mapcar #'hym-workspace-name (hym-workspace-active))
                   '("live")))))

(ert-deftest hym-workspace-load-signals-and-save-refuses-on-corrupt-file ()
  (hym-workspace-test-with-empty-registry
    (let ((garbage "(:name \"x\""))
      (with-temp-file hym-workspace-registry-file
        (insert garbage))
      (let ((hym-workspace--registry nil)
            (hym-workspace--loaded nil))
        (should-error (hym-workspace-registry))
        (should-error (hym-workspace-put '(:name "y" :type project :root "~/y"))))
      (with-temp-buffer
        (insert-file-contents hym-workspace-registry-file)
        (should (equal (buffer-string) garbage))))))

(ert-deftest hym-workspace-repo-dirs-worktree-and-project ()
  (hym-workspace-test-with-empty-registry
    (let ((wt '(:name "w" :type worktree :root "~/orca/w"
                :repos ("ploy-server" "ploy-client")))
          (pj '(:name "p" :type project :root "~/dotfiles")))
      (should (equal (hym-workspace-repo-dirs wt)
                     (list (file-name-as-directory (expand-file-name "~/orca/w/ploy-server"))
                           (file-name-as-directory (expand-file-name "~/orca/w/ploy-client")))))
      (should (equal (hym-workspace-repo-dirs pj)
                     (list (file-name-as-directory (expand-file-name "~/dotfiles"))))))))

(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))

(defun hym/tabs-test-create (name group)
  "Create a tab named NAME in GROUP."
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (tab-bar-change-tab-group group))

(defmacro hym-workspace-test-with-shell (&rest body)
  "Run BODY with an isolated tab list and an empty registry."
  (declare (indent 0) (debug t))
  `(let ((saved-tabs (frame-parameter nil 'tabs))
         (saved-hym-tabs-mode hym-tabs-mode)
         (hym/tab-group-last-tab (make-hash-table :test 'equal)))
     (hym-workspace-test-with-empty-registry
       (unwind-protect
           (progn
             (when hym-tabs-mode (hym-tabs-mode -1))
             (set-frame-parameter nil 'tabs nil)
             (hym-tabs-mode 1)
             ,@body)
         (hym-tabs-mode -1)
         (set-frame-parameter nil 'tabs saved-tabs)
         (when saved-hym-tabs-mode (hym-tabs-mode 1))))))

(ert-deftest hym-workspace-open-creates-group-and-is-open ()
  (hym-workspace-test-with-shell
    (let ((ws (hym-workspace-put '(:name "zippy" :type project :root "~"))))
      (hym-workspace-open ws)
      (should (member "zippy" (hym/tab-groups)))
      (should (hym-workspace-open-p ws))
      (should (equal (hym-workspace-name (hym-workspace-current)) "zippy")))))

(ert-deftest hym-workspace-close-removes-group-keeps-entry ()
  (hym-workspace-test-with-shell
    (let ((ws (hym-workspace-put '(:name "zippy" :type project :root "~"))))
      (hym-workspace-open ws)
      (hym-workspace-close ws)
      (should-not (member "zippy" (hym/tab-groups)))
      (should (hym-workspace-get "zippy")))))

(ert-deftest hym-workspace-open-hook-runs-in-context ()
  (hym-workspace-test-with-shell
    (let* ((seen nil)
           (hym-workspace-open-hook
            (list (lambda () (setq seen (hym-workspace-name (hym-workspace-current))))))
           (ws (hym-workspace-put '(:name "zippy" :type project :root "~"))))
      (hym-workspace-open ws)
      (should (equal seen "zippy")))))

(ert-deftest hym-workspace-spawn-tab-adds-named-tab-in-group ()
  (hym-workspace-test-with-shell
    (let ((ws (hym-workspace-put '(:name "zippy" :type project :root "~"))))
      (hym-workspace-spawn-tab ws "scratch" (lambda () (switch-to-buffer "*scratch*")))
      (should (equal (hym/tab-group) "zippy"))
      (should (equal (alist-get 'name (tab-bar--current-tab)) "scratch")))))

(ert-deftest hym-workspace-format-shows-only-current-group ()
  (hym-workspace-test-with-shell
    (tab-bar-rename-tab "A1")
    (tab-bar-change-tab-group "A")
    (hym/tabs-test-create "B1" "B")
    (hym/tabs-test-create "B2" "B")
    (hym/tab-group-switch-to "B")
    (let ((items (hym-workspace-format-current-group-tabs)))
      ;; Only B's two tabs contribute tab menu-items (one is `current-tab',
      ;; the other `tab-N'); A1 must be absent.
      (should (= 2 (seq-count
                    (lambda (it)
                      (and (consp it)
                           (symbolp (car it))
                           (let ((n (symbol-name (car it))))
                             (or (string-prefix-p "tab-" n)
                                 (equal n "current-tab")))))
                    items))))))

(ert-deftest hym-workspace-create-adds-bare-entry ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-create "dots" 'project "~/dotfiles")
    (let ((ws (hym-workspace-get "dots")))
      (should (equal (hym-workspace-type ws) 'project))
      (should (equal (hym-workspace-root ws) (expand-file-name "~/dotfiles")))
      (should-not (hym-workspace-archived-p ws)))))

(ert-deftest hym-workspace-mode-restores-format-after-double-enable ()
  (hym-workspace-test-with-shell
    (let ((saved-mode hym-workspace-mode)
          (saved-format tab-bar-format))
      (unwind-protect
          (let ((original tab-bar-format))
            (when hym-workspace-mode (hym-workspace-mode -1))
            (setq tab-bar-format original)
            (hym-workspace-mode 1)
            (hym-workspace-mode 1)
            (should (eq (car tab-bar-format) 'hym-workspace-format-current-group-tabs))
            (hym-workspace-mode -1)
            (should (equal tab-bar-format original)))
        (hym-workspace-mode (if saved-mode 1 -1))
        (setq tab-bar-format saved-format)))))

(ert-deftest hym-workspace-registry-change-hook-fires-on-put-and-remove ()
  (hym-workspace-test-with-empty-registry
    (let* ((n 0)
           (hym-workspace-registry-change-hook (list (lambda () (setq n (1+ n))))))
      (hym-workspace-put '(:name "a" :type project :root "~/a"))
      (hym-workspace-remove "a")
      (should (= n 2)))))

(ert-deftest hym-workspace-after-open-hook-fires-on-open-and-switch ()
  (hym-workspace-test-with-shell
    (let* ((n 0)
           (hym-workspace-after-open-hook (list (lambda () (setq n (1+ n)))))
           (a (hym-workspace-put '(:name "a" :type project :root "~")))
           (b (hym-workspace-put '(:name "b" :type project :root "~"))))
      (hym-workspace-open a)
      (hym-workspace-open b)
      (hym-workspace-open a)
      (should (= n 3)))))

(ert-deftest hym-workspace-switch-opens-named-workspace ()
  (hym-workspace-test-with-shell
    (hym-workspace-put '(:name "a" :type project :root "~"))
    (hym-workspace-put '(:name "b" :type project :root "~"))
    (hym-workspace-switch "b")
    (should (equal (hym/tab-group) "b"))
    (hym-workspace-switch "a")
    (should (equal (hym/tab-group) "a"))))

(ert-deftest hym-workspace-cycle-moves-between-active-and-wraps ()
  (hym-workspace-test-with-shell
    (hym-workspace-put '(:name "a" :type project :root "~"))
    (hym-workspace-put '(:name "b" :type project :root "~"))
    (hym-workspace-put '(:name "c" :type project :root "~"))
    (hym-workspace-switch "a")
    (hym-workspace-next)
    (should (equal (hym/tab-group) "b"))
    (hym-workspace-next)
    (should (equal (hym/tab-group) "c"))
    (hym-workspace-next)
    (should (equal (hym/tab-group) "a"))
    (hym-workspace-prev)
    (should (equal (hym/tab-group) "c"))))

(ert-deftest hym-workspace-select-index-opens-nth-active ()
  (hym-workspace-test-with-shell
    (hym-workspace-put '(:name "a" :type project :root "~"))
    (hym-workspace-put '(:name "b" :type project :root "~" :archived t))
    (hym-workspace-put '(:name "c" :type project :root "~"))
    (hym-workspace-select-index 2)
    (should (equal (hym/tab-group) "c"))))

(ert-deftest hym-workspace-slug-and-archived ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "w" :slug "w_slug" :type worktree :root "~/w"
                         :repos ("a") :archived t))
    (hym-workspace-put '(:name "p" :type project :root "~/p"))
    (should (equal (hym-workspace-slug (hym-workspace-get "w")) "w_slug"))
    (should (null (hym-workspace-slug (hym-workspace-get "p"))))
    (should (equal (mapcar #'hym-workspace-name (hym-workspace-archived)) '("w")))))

(ert-deftest hym-workspace-rename-preserves-position ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "a" :type project :root "~/a"))
    (hym-workspace-put '(:name "b" :type project :root "~/b"))
    (hym-workspace-rename (hym-workspace-get "a") "aa")
    (should (equal (mapcar #'hym-workspace-name (hym-workspace-registry)) '("aa" "b")))
    (should (null (hym-workspace-get "a")))))

(ert-deftest hym-workspace-rename-rejects-existing-name ()
  (hym-workspace-test-with-empty-registry
    (hym-workspace-put '(:name "a" :type project :root "~/a"))
    (hym-workspace-put '(:name "b" :type project :root "~/b"))
    (should-error (hym-workspace-rename (hym-workspace-get "a") "b"))
    (should (hym-workspace-get "a"))))

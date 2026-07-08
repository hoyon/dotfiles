;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-sidebar.el" (file-name-directory load-file-name)))

(defmacro hym-workspace-sidebar-test-with-registry (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((temp (make-temp-file "hym-ws" nil ".eld"))
          (hym-workspace-registry-file temp)
          (hym-workspace--registry nil)
          (hym-workspace--loaded t)
          (hym-workspace-sidebar--point-name nil)
          (hym-workspace-sidebar--point-line nil))
     (unwind-protect (progn ,@body)
       (when (file-exists-p temp) (delete-file temp)))))

(ert-deftest hym-workspace-sidebar-card-shows-name-and-repo-count ()
  (hym-workspace-sidebar-test-with-registry
    (let ((card (hym-workspace-sidebar--card
                 '(:name "zippy" :type worktree :root "~/orca/zippy"
                   :repos ("a" "b")))))
      (should (string-match-p "zippy" card))
      (should (string-match-p "2 repos" card))
      (should (equal (get-text-property 2 'hym-workspace card) "zippy")))))

(ert-deftest hym-workspace-sidebar-card-shows-status-badges ()
  (hym-workspace-sidebar-test-with-registry
    (let ((hym-workspace-sidebar-status-functions
           (list (lambda (ws)
                   (when (equal (hym-workspace-name ws) "srv")
                     (list "server running"))))))
      (should (string-match-p
               "server running"
               (hym-workspace-sidebar--card
                '(:name "srv" :type worktree :root "~" :repos ("a")))))
      (should-not (string-match-p
                   "server running"
                   (hym-workspace-sidebar--card
                    '(:name "other" :type project :root "~")))))))

(ert-deftest hym-workspace-sidebar-render-lists-active-only ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "live" :type project :root "~/l"))
    (hym-workspace-put '(:name "old" :type project :root "~/o" :archived t))
    (with-temp-buffer
      (hym-workspace-sidebar--render)
      (goto-char (point-min))
      (should (search-forward "live" nil t))
      (goto-char (point-min))
      (should-not (search-forward "old" nil t)))))

(ert-deftest hym-workspace-sidebar-at-point-returns-name ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "zippy" :type project :root "~/z"))
    (with-temp-buffer
      (hym-workspace-sidebar--render)
      (goto-char (point-min))
      (search-forward "zippy")
      (should (equal (hym-workspace-sidebar--at-point) "zippy")))))

(ert-deftest hym-workspace-sidebar-ensure-window-noop-when-not-visible ()
  (hym-workspace-sidebar-test-with-registry
    (let ((hym-workspace-sidebar--visible nil))
      (hym-workspace-sidebar--ensure-window)
      (should-not (get-buffer-window hym-workspace-sidebar-buffer-name t)))))

(ert-deftest hym-workspace-sidebar-ensure-window-shows-when-visible ()
  (hym-workspace-sidebar-test-with-registry
    (let ((hym-workspace-sidebar--visible t))
      (unwind-protect
          (progn
            (hym-workspace-sidebar--ensure-window)
            (should (get-buffer-window hym-workspace-sidebar-buffer-name t)))
        (when-let ((win (get-buffer-window hym-workspace-sidebar-buffer-name t)))
          (delete-window win))))))

(ert-deftest hym-workspace-sidebar-archive-marks-and-hides ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "z" :type project :root "~/z"))
    (with-temp-buffer
      (hym-workspace-sidebar-mode)
      (hym-workspace-sidebar--render)
      (goto-char (point-min))
      (search-forward "z")
      (hym-workspace-sidebar-archive)
      (should (hym-workspace-archived-p (hym-workspace-get "z")))
      (goto-char (point-min))
      (should-not (search-forward "z" nil t)))))

(ert-deftest hym-workspace-sidebar-render-keeps-point-on-workspace ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "a" :type project :root "~"))
    (hym-workspace-put '(:name "b" :type project :root "~"))
    (hym-workspace-put '(:name "c" :type project :root "~"))
    (with-temp-buffer
      (hym-workspace-sidebar--render)
      (goto-char (point-min))
      (search-forward "b")
      (beginning-of-line)
      (should (equal (hym-workspace-sidebar--at-point) "b"))
      (hym-workspace-sidebar--render)
      (should (equal (hym-workspace-sidebar--at-point) "b")))))

(ert-deftest hym-workspace-sidebar-render-honours-remembered-point ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "a" :type project :root "~"))
    (hym-workspace-put '(:name "b" :type project :root "~"))
    (with-temp-buffer
      (let ((hym-workspace-sidebar--point-name "b"))
        (hym-workspace-sidebar--render)
        (should (equal (hym-workspace-sidebar--at-point) "b"))))))

(ert-deftest hym-workspace-sidebar-render-keeps-exact-clicked-line ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "a" :type worktree :root "~" :repos ("x")))
    (hym-workspace-put '(:name "b" :type worktree :root "~" :repos ("y")))
    (with-temp-buffer
      (hym-workspace-sidebar--render)
      (goto-char (point-min))
      (search-forward "b")
      (forward-line 1)
      (should (equal (hym-workspace-sidebar--at-point) "b"))
      (let ((meta (buffer-substring (line-beginning-position) (line-end-position)))
            (hym-workspace-sidebar--point-name "b")
            (hym-workspace-sidebar--point-line (line-number-at-pos)))
        (should (string-match-p "worktree" meta))
        (hym-workspace-sidebar--render)
        (should (equal (buffer-substring (line-beginning-position) (line-end-position))
                       meta))))))

(ert-deftest hym-workspace-sidebar-line-marks-current-workspace ()
  (hym-workspace-sidebar-test-with-registry
    (let ((saved-tabs (frame-parameter nil 'tabs))
          (saved-mode hym-tabs-mode)
          (hym/tab-group-last-tab (make-hash-table :test 'equal)))
      (unwind-protect
          (progn
            (when hym-tabs-mode (hym-tabs-mode -1))
            (set-frame-parameter nil 'tabs nil)
            (hym-tabs-mode 1)
            (tab-bar-change-tab-group "cur")
            (hym-workspace-put '(:name "cur" :type project :root "~"))
            (hym-workspace-put '(:name "other" :type project :root "~"))
            (should (string-match-p "▸" (hym-workspace-sidebar--card (hym-workspace-get "cur"))))
            (should-not (string-match-p "▸" (hym-workspace-sidebar--card (hym-workspace-get "other")))))
        (hym-tabs-mode -1)
        (set-frame-parameter nil 'tabs saved-tabs)
        (when saved-mode (hym-tabs-mode 1))))))

(ert-deftest hym-workspace-sidebar-auto-refreshes-on-registry-change ()
  (hym-workspace-sidebar-test-with-registry
    (let ((buf (get-buffer-create hym-workspace-sidebar-buffer-name)))
      (unwind-protect
          (with-current-buffer buf
            (hym-workspace-sidebar-mode)
            (hym-workspace-sidebar--render)
            (goto-char (point-min))
            (should-not (search-forward "fresh" nil t))
            (hym-workspace-put '(:name "fresh" :type project :root "~/f"))
            (goto-char (point-min))
            (should (search-forward "fresh" nil t)))
        (kill-buffer buf)))))

(ert-deftest hym-workspace-sidebar-archived-section-toggles ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "live" :type project :root "~/l"))
    (hym-workspace-put '(:name "gone" :type project :root "~/g" :archived t))
    (with-temp-buffer
      (let ((hym-workspace-sidebar--show-archived nil))
        (hym-workspace-sidebar--render)
        (goto-char (point-min))
        (should-not (search-forward "gone" nil t)))
      (let ((hym-workspace-sidebar--show-archived t))
        (hym-workspace-sidebar--render)
        (goto-char (point-min))
        (should (search-forward "ARCHIVED" nil t))
        (should (search-forward "gone" nil t))))))

(ert-deftest hym-workspace-sidebar-rename-follows-renamed-workspace ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "old" :type project :root "~/o"))
    (with-temp-buffer
      (hym-workspace-sidebar-mode)
      (hym-workspace-sidebar--render)
      (hym-workspace-sidebar--goto-workspace "old")
      (hym-workspace-sidebar-rename "new")
      (should (equal hym-workspace-sidebar--point-name "new"))
      (should (hym-workspace-get "new"))
      (should-not (hym-workspace-get "old")))))

(ert-deftest hym-workspace-sidebar-unarchive-soft-for-non-worktree ()
  (hym-workspace-sidebar-test-with-registry
    (hym-workspace-put '(:name "p" :type project :root "~/p" :archived t))
    (let ((hym-workspace-sidebar--show-archived t))
      (with-temp-buffer
        (hym-workspace-sidebar-mode)
        (hym-workspace-sidebar--render)
        (hym-workspace-sidebar--goto-workspace "p")
        (hym-workspace-sidebar-unarchive)
        (should-not (hym-workspace-archived-p (hym-workspace-get "p")))))))

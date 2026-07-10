;; -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))

(defmacro hym/tabs-test-with-clean-frame (&rest body)
  "Run BODY with an isolated tab list on the selected frame."
  (declare (indent 0) (debug t))
  `(let ((saved-tabs (frame-parameter nil 'tabs))
         (saved-tab-bar-mode tab-bar-mode)
         (saved-history-mode tab-bar-history-mode)
         (saved-hym-tabs-mode hym-tabs-mode)
         (hym/tab-group-last-tab (make-hash-table :test 'equal)))
     (unwind-protect
         (progn
           (when hym-tabs-mode
             (hym-tabs-mode -1))
           (set-frame-parameter nil 'tabs nil)
           (tab-bar-history-mode 1)
           (hym-tabs-mode 1)
           ,@body)
       (hym-tabs-mode -1)
       (tab-bar-history-mode (if saved-history-mode 1 -1))
       (set-frame-parameter nil 'tabs saved-tabs)
       (tab-bar-mode (if saved-tab-bar-mode 1 -1))
       (when saved-hym-tabs-mode
         (hym-tabs-mode 1)))))

(defun hym/tabs-test-create (name group)
  "Create a tab named NAME in GROUP."
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (tab-bar-change-tab-group group))

(defun hym/tabs-test-current-name ()
  "Return the current tab name."
  (alist-get 'name (tab-bar--current-tab)))

(ert-deftest hym/tab-new-inherits-current-group ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-change-tab-group "workspace")
    (tab-new)
    (should (equal (hym/tab-group) "workspace"))))

(ert-deftest hym/tab-default-group-is-zero-and-other-groups-start-at-one ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-change-tab-group hym/default-tab-group)
    (should (string-match-p
             "┃0 general┃"
             (tab-bar-tab-group-format-default (tab-bar--current-tab) 1 t)))
    (hym/tabs-test-create "workspace" "workspace")
    (should (string-match-p
             "┃1 workspace┃"
             (tab-bar-tab-group-format-default (tab-bar--current-tab) 2 t)))))

(ert-deftest hym/tab-switch-to-default-group-restores-or-creates-it ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-change-tab-group "workspace")
    (hym/tab-switch-to-default-group)
    (should (equal (hym/tab-group) hym/default-tab-group))
    (let ((general-id (hym/tab-id (tab-bar--current-tab-find))))
      (hym/tabs-test-create "workspace-2" "workspace")
      (hym/tab-switch-to-default-group)
      (should (eq (alist-get 'hym-id (tab-bar--current-tab)) general-id)))))

(ert-deftest hym/open-dir-standalone-command-targets-default-group ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-change-tab-group "workspace")
    (cl-letf (((symbol-function 'dired) #'ignore)
              ((symbol-function 'hym/raise-frame) #'ignore))
      (hym/open-dir-in-tab-group temporary-file-directory "workspace"))
    (should (equal (hym/tab-group) hym/default-tab-group))))

(ert-deftest hym/git-delta-diff-refs-targets-default-group ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-change-tab-group "workspace")
    (let ((buffer (get-buffer-create " *hym-tabs-test-diff*")))
      (unwind-protect
          (cl-letf (((symbol-function 'executable-find) (lambda (_) t))
                    ((symbol-function 'hym/git-delta-diff-buffer)
                     (lambda (&rest _) buffer))
                    ((symbol-function 'hym/git-delta-diff-refresh) #'ignore)
                    ((symbol-function 'hym/raise-frame) #'ignore))
            (hym/git-delta-diff-refs
             "main" "HEAD" "workspace" temporary-file-directory)
            (should (equal (hym/tab-group) hym/default-tab-group)))
        (kill-buffer buffer)))))

(ert-deftest hym/tab-close-stays-in-group ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-rename-tab "A1")
    (tab-bar-change-tab-group "A")
    (hym/tabs-test-create "B1" "B")
    (hym/tabs-test-create "B2" "B")

    (hym/tab-group-switch-to "A")
    (hym/tab-group-switch-to "B")
    (tab-close)
    (should (equal (hym/tabs-test-current-name) "B1"))
    (hym/tab-group-switch-to "A")
    (hym/tab-group-switch-to "B")
    (should (equal (hym/tabs-test-current-name) "B1"))

    (hym/tabs-test-create "B2" "B")
    (tab-bar-select-tab 2)
    (tab-close)
    (should (equal (hym/tabs-test-current-name) "B2"))))

(ert-deftest hym/tab-group-restores-duplicate-name-by-id ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-rename-tab "A1")
    (tab-bar-change-tab-group "A")
    (hym/tabs-test-create "same" "B")
    (hym/tabs-test-create "same" "B")
    (let ((expected-id (hym/tab-id (tab-bar--current-tab-find))))
      (hym/tab-group-switch-to "A")
      (hym/tab-group-switch-to "B")
      (should (eq (alist-get 'hym-id (tab-bar--current-tab))
                  expected-id)))))

(ert-deftest hym/tab-close-last-in-group-may-leave-group ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-rename-tab "A1")
    (tab-bar-change-tab-group "A")
    (hym/tabs-test-create "B1" "B")
    (tab-close)
    (should-not (equal (hym/tab-group) "B"))))

(ert-deftest hym/tab-prevented-close-preserves-selection ()
  (hym/tabs-test-with-clean-frame
    (tab-bar-rename-tab "A1")
    (tab-bar-change-tab-group "A")
    (hym/tabs-test-create "B1" "B")
    (hym/tabs-test-create "B2" "B")
    (let ((current-id (hym/tab-id (tab-bar--current-tab-find)))
          (tab-bar-tab-prevent-close-functions (list (lambda (&rest _) t))))
      (tab-close)
      (should (= (length (funcall tab-bar-tabs-function)) 3))
      (should (eq (alist-get 'hym-id (tab-bar--current-tab))
                  current-id))
      (should (equal (hym/tabs-test-current-name) "B2")))))

;; -*- lexical-binding: t -*-

(require 'ert)
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

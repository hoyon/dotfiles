;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "tabs.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-notes.el" (file-name-directory load-file-name)))

(ert-deftest hym-workspace-notes-scratch-paths-by-key ()
  (let ((hym-workspace-home "~/workspaces"))
    ;; worktree: key is the slug
    (should (equal (hym-workspace--notes-file
                    '(:name "auth refactor" :slug "auth_refactor"
                      :type worktree :root "~/x"))
                   (expand-file-name "auth_refactor/notes.org"
                                     (expand-file-name "~/workspaces"))))
    ;; project: key is the slugified name
    (should (equal (hym-workspace--scratch-file
                    '(:name "Dot Files" :type project :root "~/dotfiles"))
                   (expand-file-name "dot_files/scratch"
                                     (expand-file-name "~/workspaces"))))))

(ert-deftest hym-workspace-notes-reuses-existing-tab ()
  (let* ((temp-eld (make-temp-file "hym-ws" nil ".eld"))
         (hym-workspace-registry-file temp-eld)
         (hym-workspace--registry nil)
         (hym-workspace--loaded t)
         (hym-workspace-home (make-temp-file "hym-home" t))
         (saved-tabs (frame-parameter nil 'tabs))
         (saved-mode hym-tabs-mode)
         (hym/tab-group-last-tab (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (when hym-tabs-mode (hym-tabs-mode -1))
          (set-frame-parameter nil 'tabs nil)
          (hym-tabs-mode 1)
          (hym-workspace-put '(:name "w" :type project :root "~"))
          (hym-workspace-open (hym-workspace-get "w"))
          (let ((home-tab (tab-bar--current-tab-index)))
            (hym-workspace-notes)
            (should (equal (alist-get 'name (tab-bar--current-tab)) "notes"))
            (let ((n (length (funcall tab-bar-tabs-function))))
              (tab-bar-select-tab (1+ home-tab))
              (hym-workspace-notes)
              (should (= n (length (funcall tab-bar-tabs-function))))
              (should (equal (alist-get 'name (tab-bar--current-tab)) "notes")))))
      (hym-tabs-mode -1)
      (set-frame-parameter nil 'tabs saved-tabs)
      (when saved-mode (hym-tabs-mode 1))
      (delete-directory hym-workspace-home t)
      (when (file-exists-p temp-eld) (delete-file temp-eld)))))

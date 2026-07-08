;; -*- lexical-binding: t -*-

(require 'ert)
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-git.el" (file-name-directory load-file-name)))

(ert-deftest hym-workspace-pick-repo-single-and-project ()
  (should (equal (hym-workspace--pick-repo
                  '(:name "w" :type worktree :root "~/orca/w" :repos ("only")))
                 (file-name-as-directory
                  (expand-file-name "only" (expand-file-name "~/orca/w")))))
  (should (equal (hym-workspace--pick-repo
                  '(:name "p" :type project :root "~/dotfiles"))
                 (file-name-as-directory (expand-file-name "~/dotfiles")))))

(ert-deftest hym-workspace-pick-repo-prompts-when-multiple ()
  (let ((native-comp-enable-subr-trampolines nil)
        (orig (symbol-function 'completing-read)))
    (unwind-protect
        (progn
          (fset 'completing-read (lambda (&rest _) "b"))
          (should (equal (hym-workspace--pick-repo
                          '(:name "w" :type worktree :root "~/orca/w"
                            :repos ("a" "b")))
                         (file-name-as-directory
                          (expand-file-name "b" (expand-file-name "~/orca/w"))))))
      (fset 'completing-read orig))))

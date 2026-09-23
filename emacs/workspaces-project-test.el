;; -*- lexical-binding: t -*-

(require 'ert)
;; `make' puts this directory on `load-path', where the config's own
;; project.el would shadow the built-in library.
(let ((load-path (seq-remove (lambda (d) (file-equal-p d default-directory))
                             load-path)))
  (require 'project))
(load-file (expand-file-name "workspaces.el" (file-name-directory load-file-name)))
(load-file (expand-file-name "workspaces-project.el" (file-name-directory load-file-name)))

(defmacro hym/workspace-project-test--with-registry (registry &rest body)
  (declare (indent 1))
  `(let ((hym-workspace--registry ,registry)
         (hym-workspace--loaded t))
     ,@body))

(ert-deftest hym/workspace-at-matches-worktree-root-and-subdirs ()
  (hym/workspace-project-test--with-registry
      '((:name "ws" :type worktree :root "/tmp/ws" :repos ("api" "web")))
    (should (equal (hym-workspace-name (hym/workspace-at "/tmp/ws")) "ws"))
    (should (equal (hym-workspace-name (hym/workspace-at "/tmp/ws/api/lib/")) "ws"))
    (should-not (hym/workspace-at "/tmp/ws2/"))
    (should-not (hym/workspace-at "/tmp/"))))

(ert-deftest hym/workspace-at-ignores-single-repo-types ()
  (hym/workspace-project-test--with-registry
      '((:name "dots" :type project :root "/tmp/dots")
        (:name "infra" :type directory :root "/tmp/infra"))
    (should-not (hym/workspace-at "/tmp/dots/emacs/"))
    (should-not (hym/workspace-at "/tmp/infra/"))))

(ert-deftest hym/workspace-search-root-falls-back-to-project ()
  (hym/workspace-project-test--with-registry
      '((:name "ws" :type worktree :root "/tmp/ws"))
    (let ((default-directory "/tmp/ws/api/"))
      (should (equal (hym/workspace-search-root) "/tmp/ws/")))
    (let ((default-directory "/tmp/elsewhere/"))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) '(transient . "/tmp/elsewhere/"))))
        (should (equal (hym/workspace-search-root) "/tmp/elsewhere/"))))))

(ert-deftest hym/workspace-project-files-spans-repos-and-root ()
  (skip-unless (and (executable-find "fd") (executable-find "git")))
  (let ((root (file-name-as-directory (make-temp-file "hym-ws" t))))
    (unwind-protect
        (let ((default-directory root))
          (write-region "" nil (expand-file-name "notes.org" root))
          (dolist (repo '("api" "web"))
            (let ((dir (expand-file-name repo root)))
              (make-directory (expand-file-name "_build" dir) t)
              (call-process "git" nil nil nil "init" "-q" dir)
              (write-region "_build/\n" nil (expand-file-name ".gitignore" dir))
              (write-region "" nil (expand-file-name "main.ex" dir))
              (write-region "" nil (expand-file-name "_build/junk" dir))))
          (let ((project (cons 'hym-workspace root)))
            (should (equal (sort (let ((project-files-relative-names t))
                                   (project-files project))
                                 #'string<)
                           '("api/.gitignore" "api/main.ex" "notes.org"
                             "web/.gitignore" "web/main.ex")))
            (should (member (expand-file-name "api/main.ex" root)
                            (project-files project)))))
      (delete-directory root t))))

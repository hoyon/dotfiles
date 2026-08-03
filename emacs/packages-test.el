;; -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "packages.el" (file-name-directory load-file-name)))

(defun hym/packages-test-row (&rest args)
  "Build a row with sensible defaults, overridden by ARGS."
  (let ((plist (list :repo "demo" :packages '("demo")
                     :installed "aaaa111" :installed-desc "v1.0.0"
                     :pinned "aaaa111" :target "aaaa111"
                     :target-desc "v1.0.0" :kind 'release
                     :ahead 0 :behind 0)))
    (cl-loop for (key value) on args by #'cddr
             do (setq plist (plist-put plist key value)))
    (apply #'hym/packages-row-create plist)))

(ert-deftest hym/packages-status-unknown-before-fetch ()
  (should (eq 'unknown (hym/packages--status
                        (hym/packages-test-row :target nil)))))

(ert-deftest hym/packages-status-up-to-date ()
  (should (eq 'up-to-date (hym/packages--status (hym/packages-test-row)))))

(ert-deftest hym/packages-status-new-release ()
  (should (eq 'new-release
              (hym/packages--status
               (hym/packages-test-row :target "bbbb222" :target-desc "v1.1.0"
                                      :kind 'release :behind 7 :ahead 0)))))

(ert-deftest hym/packages-status-new-commits ()
  (should (eq 'new-commits
              (hym/packages--status
               (hym/packages-test-row :target "bbbb222" :target-desc "bbbb222"
                                      :kind 'commit :behind 3 :ahead 0)))))

(ert-deftest hym/packages-status-past-release ()
  (should (eq 'past-release
              (hym/packages--status
               (hym/packages-test-row :target "bbbb222" :target-desc "v1.0.0"
                                      :kind 'release :behind 0 :ahead 12)))))

(ert-deftest hym/packages-status-unknown-count-still-offers-update ()
  "A shallow clone can fail to count; the update is still available."
  (should (eq 'new-commits
              (hym/packages--status
               (hym/packages-test-row :target "bbbb222" :kind 'commit
                                      :behind nil :ahead nil)))))

(ert-deftest hym/packages-updatable-only-when-moving-forward ()
  (should (hym/packages--updatable-p
           (hym/packages-test-row :target "bbbb222" :kind 'release :behind 7)))
  (should-not (hym/packages--updatable-p (hym/packages-test-row)))
  (should-not (hym/packages--updatable-p
               (hym/packages-test-row :target "bbbb222" :behind 0 :ahead 12)))
  (should-not (hym/packages--updatable-p (hym/packages-test-row :target nil))))

(ert-deftest hym/packages-pin-note-reports-lockfile-disagreement ()
  (should-not (hym/packages--pin-note (hym/packages-test-row)))
  (should (equal "unpinned" (hym/packages--pin-note
                             (hym/packages-test-row :pinned nil))))
  (should (equal "drifted" (hym/packages--pin-note
                            (hym/packages-test-row :pinned "cccc333")))))

(ert-deftest hym/packages-status-label-includes-counts-and-notes ()
  (should (equal "up to date" (hym/packages--status-label
                               (hym/packages-test-row))))
  (should (equal "3 new commits"
                 (hym/packages--status-label
                  (hym/packages-test-row :target "bbbb222" :kind 'commit
                                         :behind 3))))
  (should (equal "1 new commit"
                 (hym/packages--status-label
                  (hym/packages-test-row :target "bbbb222" :kind 'commit
                                         :behind 1))))
  (should (equal "new release"
                 (hym/packages--status-label
                  (hym/packages-test-row :target "bbbb222" :kind 'release
                                         :behind 7))))
  (should (equal "12 past release"
                 (hym/packages--status-label
                  (hym/packages-test-row :target "bbbb222" :behind 0
                                         :ahead 12))))
  (should (equal "up to date, drifted"
                 (hym/packages--status-label
                  (hym/packages-test-row :pinned "cccc333")))))

(ert-deftest hym/packages-lockfile-round-trips ()
  (let ((path (make-temp-file "hym-packages-lock" nil ".el"))
        (alist '(("zebra" . "3333cccc") ("alpha" . "1111aaaa")
                 ("middle" . "2222bbbb"))))
    (unwind-protect
        (progn
          (hym/packages--lockfile-write alist path)
          (should (equal '(("alpha" . "1111aaaa")
                           ("middle" . "2222bbbb")
                           ("zebra" . "3333cccc"))
                         (hym/packages--lockfile-read path)))
          (with-temp-buffer
            (insert-file-contents path)
            (should (string-suffix-p ")\n:epsilon\n" (buffer-string)))))
      (delete-file path))))

(ert-deftest hym/packages-lockfile-read-missing-file-is-nil ()
  (should-not (hym/packages--lockfile-read
               (expand-file-name "definitely-absent.el"
                                 temporary-file-directory))))

(ert-deftest hym/packages-select-target-prefers-a-reachable-tag ()
  (should (equal '("dddd444" "v2.3.0" release)
                 (hym/packages--select-target "eeee555" "v2.3.0" "dddd444"))))

(ert-deftest hym/packages-select-target-falls-back-to-the-tip ()
  "Repos with no tags, or tags unreachable from the tip, track the branch."
  (should (equal '("eeee5551234567" "eeee555" commit)
                 (hym/packages--select-target "eeee5551234567" nil nil)))
  (should (equal '("eeee5551234567" "eeee555" commit)
                 (hym/packages--select-target "eeee5551234567" "v2.3.0" nil))))

(ert-deftest hym/packages-select-target-without-a-tip-is-empty ()
  (should (equal '(nil nil commit) (hym/packages--select-target nil nil nil))))

(ert-deftest hym/packages-parse-counts-reads-left-right-output ()
  (should (equal '(12 . 0) (hym/packages--parse-counts "12\t0")))
  (should (equal '(0 . 7) (hym/packages--parse-counts "0\t7\n"))))

(ert-deftest hym/packages-parse-counts-tolerates-failure ()
  "Shallow clones make rev-list fail; that must not break the row."
  (should (equal '(nil . nil) (hym/packages--parse-counts nil)))
  (should (equal '(nil . nil) (hym/packages--parse-counts "")))
  (should (equal '(nil . nil) (hym/packages--parse-counts "garbage"))))

(ert-deftest hym/packages-queue-respects-the-concurrency-cap ()
  "Twenty jobs with a cap of three must never have four in flight."
  (let* ((started 0) (peak 0) (live 0) (callbacks nil)
         (jobs (mapcar (lambda (_i)
                         (lambda (done)
                           (cl-incf started)
                           (cl-incf live)
                           (setq peak (max peak live))
                           (push (lambda () (cl-decf live) (funcall done))
                                 callbacks)))
                       (number-sequence 1 20))))
    (hym/packages--run-queue jobs 3)
    (should (= 3 started))
    ;; Drain one at a time; each completion should admit exactly one more job.
    (while callbacks
      (funcall (pop callbacks)))
    (should (= 20 started))
    (should (= 3 peak))
    (should (= 0 live))))

(ert-deftest hym/packages-queue-handles-fewer-jobs-than-the-cap ()
  (let ((started 0))
    (hym/packages--run-queue
     (list (lambda (done) (cl-incf started) (funcall done))
           (lambda (done) (cl-incf started) (funcall done)))
     8)
    (should (= 2 started))))

(ert-deftest hym/packages-queue-with-no-jobs-does-nothing ()
  (should-not (hym/packages--run-queue nil 8)))

(ert-deftest hym/packages-loaded-files-selects-only-that-directory ()
  (let ((load-history '(("/build/magit/magit.elc" . nil)
                        ("/build/magit/magit-diff.elc" . nil)
                        ("/build/consult/consult.elc" . nil)
                        (require . some-symbol))))
    (should (equal '("/build/magit/magit.elc" "/build/magit/magit-diff.elc")
                   (hym/packages--loaded-files "/build/magit")))
    (should-not (hym/packages--loaded-files "/build/vertico"))))

(ert-deftest hym/packages-scan-config-maps-packages-to-their-file ()
  (let ((dir (make-temp-file "hym-packages-config" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "git.el" dir)
            (insert ";; -*- lexical-binding: t -*-\n"
                    "(use-package magit\n  :config\n  (setq x 1))\n"
                    "(use-package forge :after magit)\n"))
          (with-temp-file (expand-file-name "vertico.el" dir)
            (insert "(use-package vertico\n  :init (vertico-mode))\n"
                    ";; (use-package not-real) inside a comment\n"))
          (let ((map (hym/packages--scan-config dir)))
            (should (equal (expand-file-name "git.el" dir)
                           (cdr (assoc "magit" map))))
            (should (equal (expand-file-name "git.el" dir)
                           (cdr (assoc "forge" map))))
            (should (equal (expand-file-name "vertico.el" dir)
                           (cdr (assoc "vertico" map))))
            (should-not (assoc "not-real" map))))
      (delete-directory dir t))))

(ert-deftest hym/packages-reloadable-p-honours-the-blocklist ()
  (let ((hym/packages-no-reload '("evil" "compat")))
    (should (hym/packages--reloadable-p (hym/packages-test-row :repo "magit")))
    (should-not (hym/packages--reloadable-p
                 (hym/packages-test-row :repo "evil")))))

(ert-deftest hym/packages-lockfile-update-merges-and-preserves-others ()
  (let ((path (make-temp-file "hym-packages-lock" nil ".el")))
    (unwind-protect
        (progn
          (hym/packages--lockfile-write
           '(("magit" . "1111aaaa") ("consult" . "2222bbbb")) path)
          (hym/packages--lockfile-update
           '(("magit" . "9999zzzz") ("brand-new" . "8888yyyy")) path)
          (should (equal '(("brand-new" . "8888yyyy")
                           ("consult" . "2222bbbb")
                           ("magit" . "9999zzzz"))
                         (hym/packages--lockfile-read path))))
      (delete-file path))))

(ert-deftest hym/packages-summary-names-restart-required-packages ()
  "A package needing a restart was still updated, so it appears in both lists."
  (should (equal "Updated magit v4.5.0, evil 9f2e1d0. Restart needed for: evil."
                 (hym/packages--summary
                  '((:repo "magit" :desc "v4.5.0" :reloaded t)
                    (:repo "evil" :desc "9f2e1d0" :reloaded nil)))))
  (should (equal "Updated magit v4.5.0, consult 3.7."
                 (hym/packages--summary
                  '((:repo "magit" :desc "v4.5.0" :reloaded t)
                    (:repo "consult" :desc "3.7" :reloaded t)))))
  (should (equal "Nothing to update." (hym/packages--summary nil))))

(ert-deftest hym/packages-summary-reports-failures-alongside-successes ()
  "A package that errored must be named, not silently dropped from the report."
  (should (equal "Updated magit v4.5.0. Failed: ghostel (no such file)."
                 (hym/packages--summary
                  '((:repo "magit" :desc "v4.5.0" :reloaded t)
                    (:repo "ghostel" :desc "v0.49.0" :error "no such file")))))
  (should (equal "Failed: ghostel (boom)."
                 (hym/packages--summary
                  '((:repo "ghostel" :desc "v0.49.0" :error "boom")))))
  (should (equal "Updated a 1, b 2. Restart needed for: b. Failed: c (bang)."
                 (hym/packages--summary
                  '((:repo "a" :desc "1" :reloaded t)
                    (:repo "b" :desc "2" :reloaded nil)
                    (:repo "c" :desc "3" :error "bang"))))))

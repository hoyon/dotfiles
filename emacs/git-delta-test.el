;; -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(defmacro hym/leader-def (&rest _))
(defmacro general-define-key (&rest _))
(load-file (expand-file-name "git-delta.el" (file-name-directory load-file-name)))

(ert-deftest hym/git-delta-parse-file-header-modified ()
  (should (equal (hym/git-delta-diff--parse-file-header "lib/foo.ex")
                 '("lib/foo.ex" . modified))))

(ert-deftest hym/git-delta-parse-file-header-added ()
  (should (equal (hym/git-delta-diff--parse-file-header "added: lib/new.ex")
                 '("lib/new.ex" . added))))

(ert-deftest hym/git-delta-parse-file-header-removed ()
  (should (equal (hym/git-delta-diff--parse-file-header "removed: lib/old.ex")
                 '("lib/old.ex" . removed))))

(ert-deftest hym/git-delta-parse-file-header-renamed ()
  (should (equal (hym/git-delta-diff--parse-file-header "renamed: a/old.ex ⟶   b/new.ex")
                 '("b/new.ex" . renamed))))

(ert-deftest hym/git-delta-hunk-line-number ()
  (should (= (hym/git-delta-diff--hunk-line-number "42: def foo(x) ") 42))
  (should (= (hym/git-delta-diff--hunk-line-number "1: ") 1))
  (should-not (hym/git-delta-diff--hunk-line-number "lib/foo.ex"))
  (should-not (hym/git-delta-diff--hunk-line-number "│  42 │ x: 1        │  42 │ x: 1")))

(ert-deftest hym/git-delta-right-line-number ()
  (should (= (hym/git-delta-diff--right-line-number
              "│  41 │ context      │  41 │ context")
             41))
  (should (= (hym/git-delta-diff--right-line-number
              "│     │              │  42 │ added")
             42))
  (should-not (hym/git-delta-diff--right-line-number
               "│  42 │ removed      │     │"))
  (should-not (hym/git-delta-diff--right-line-number "lib/foo.ex")))

(defconst hym/git-delta-test-fixture
  " f1.txt | 4 ++--
 f3.txt | 1 +
 2 files changed, 3 insertions(+), 2 deletions(-)

f1.txt
────────────────────────────────────────────────────────────────────────────────

1: 
────────────────────────────────────────────────────────────────────────────────
│  1 │a                                 │  1 │a
│  2 │b                                 │    │
│    │                                  │  2 │B
│  3 │c                                 │  3 │c

9: h 
────────────────────────────────────────────────────────────────────────────────
│ 10 │j                                 │ 10 │j
│ 11 │k                                 │    │
│    │                                  │ 11 │K

added: f3.txt
────────────────────────────────────────────────────────────────────────────────

1: 
────────────────────────────────────────────────────────────────────────────────
│    │                                  │  1 │new
")

(defmacro hym/git-delta-test-with-fixture (&rest body)
  (declare (indent 0) (debug t))
  `(with-temp-buffer
     (insert hym/git-delta-test-fixture)
     (hym/git-delta-diff--mark-headings)
     (goto-char (point-min))
     ,@body))

(defun hym/git-delta-test-goto-line-matching (regexp)
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line))

(ert-deftest hym/git-delta-mark-headings-file ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (should (= (get-text-property (point) 'hym/git-delta-heading) 1))
    (should (equal (get-text-property (point) 'hym/git-delta-file)
                   '("f3.txt" . added)))))

(ert-deftest hym/git-delta-mark-headings-hunk ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^9: h")
    (should (= (get-text-property (point) 'hym/git-delta-heading) 2))
    (should (= (get-text-property (point) 'hym/git-delta-hunk) 9))
    (should-not (get-text-property (point) 'hym/git-delta-file))))

(ert-deftest hym/git-delta-mark-headings-ignores-stat-and-code-lines ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^ f1.txt | 4")
    (should-not (get-text-property (point) 'hym/git-delta-heading))
    (hym/git-delta-test-goto-line-matching "^│  1 │a")
    (should-not (get-text-property (point) 'hym/git-delta-heading))
    (hym/git-delta-test-goto-line-matching "^───")
    (should-not (get-text-property (point) 'hym/git-delta-heading))))

(ert-deftest hym/git-delta-imenu-index-lists-files-in-order ()
  (hym/git-delta-test-with-fixture
    (let ((index (hym/git-delta-diff--imenu-index)))
      (should (equal (mapcar #'car index) '("f1.txt" "f3.txt")))
      (goto-char (cdr (assoc "f3.txt" index)))
      (should (looking-at "added: f3.txt")))))

(ert-deftest hym/git-delta-location-on-context-line ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^│  3 │c")
    (should (equal (hym/git-delta-diff--location-at-point)
                   '("f1.txt" modified 3)))))

(ert-deftest hym/git-delta-location-on-removed-line-uses-previous-right-number ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^│ 11 │k")
    (should (equal (hym/git-delta-diff--location-at-point)
                   '("f1.txt" modified 10)))))

(ert-deftest hym/git-delta-location-falls-back-to-hunk-start ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^9: h")
    (forward-line 1)
    (should (equal (hym/git-delta-diff--location-at-point)
                   '("f1.txt" modified 9)))))

(ert-deftest hym/git-delta-location-on-file-header ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^added: f3.txt")
    (should (equal (hym/git-delta-diff--location-at-point)
                   '("f3.txt" added 1)))))

(ert-deftest hym/git-delta-location-before-first-file-is-nil ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^ 2 files changed")
    (should-not (hym/git-delta-diff--location-at-point))))

(defmacro hym/git-delta-test-with-rendered-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(let ((fixture-file (make-temp-file "delta-fixture")))
     (unwind-protect
         (progn
           (with-temp-file fixture-file (insert hym/git-delta-test-fixture))
           (with-temp-buffer
             (hym/git-delta-diff-mode)
             (setq-local hym/git-delta-diff--directory default-directory)
             (setq-local hym/git-delta-diff--command-fn
                         (lambda () (format "cat %s" (shell-quote-argument fixture-file))))
             (hym/git-delta-diff-refresh)
             ,@body))
       (delete-file fixture-file))))

(defun hym/git-delta-test-collapsed-p (regexp)
  (hym/git-delta-test-goto-line-matching regexp)
  (outline-invisible-p (line-end-position)))

(ert-deftest hym/git-delta-refresh-marks-headings-and-enables-outline ()
  (hym/git-delta-test-with-rendered-buffer
    (should outline-minor-mode)
    (hym/git-delta-test-goto-line-matching "^f1.txt$")
    (should (outline-on-heading-p))
    (should (= (funcall outline-level) 1))
    (hym/git-delta-test-goto-line-matching "^9: h")
    (should (= (funcall outline-level) 2))))

(ert-deftest hym/git-delta-refresh-preserves-folds ()
  (hym/git-delta-test-with-rendered-buffer
    (hym/git-delta-test-goto-line-matching "^f1.txt$")
    (outline-hide-subtree)
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (re-search-forward "^1: $")
    (outline-hide-subtree)
    (hym/git-delta-diff-refresh)
    (should (hym/git-delta-test-collapsed-p "^f1.txt$"))
    (should-not (hym/git-delta-test-collapsed-p "^added: f3.txt$"))
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (re-search-forward "^1: $")
    (should (outline-invisible-p (line-end-position)))))

(ert-deftest hym/git-delta-delta-command-uses-underlined-hunk-headers ()
  (let ((command (hym/git-delta-diff--delta-command 120)))
    (should (string-match-p "--side-by-side" command))
    (should (string-match-p "--hunk-header-decoration-style underline" command))
    (should (string-match-p "--width 120" command))))

(defmacro hym/git-delta-test-with-find-file-stub (visited &rest body)
  (declare (indent 1) (debug t))
  `(let ((target (generate-new-buffer "target")))
     (unwind-protect
         (with-current-buffer target
           (insert "l1\nl2\nl3\nl4\nl5\n")
           (cl-letf (((symbol-function 'find-file)
                      (lambda (path)
                        (push path ,visited)
                        (switch-to-buffer target))))
             ,@body))
       (kill-buffer target))))

(ert-deftest hym/git-delta-visit-file-opens-at-right-pane-line ()
  (let (visited)
    (hym/git-delta-test-with-find-file-stub visited
      (hym/git-delta-test-with-fixture
        (setq-local hym/git-delta-diff--directory "/repo/")
        (hym/git-delta-test-goto-line-matching "^│  3 │c")
        (hym/git-delta-diff-visit-file)
        (should (equal visited '("/repo/f1.txt")))
        (with-current-buffer "target"
          (should (= (line-number-at-pos) 3)))))))

(ert-deftest hym/git-delta-visit-file-rejects-removed-file ()
  (let (visited)
    (hym/git-delta-test-with-find-file-stub visited
      (with-temp-buffer
        (insert "removed: gone.ex\n────────\n\n1: \n────\n│  1 │x   │    │\n")
        (hym/git-delta-diff--mark-headings)
        (setq-local hym/git-delta-diff--directory "/repo/")
        (goto-char (point-max))
        (should-error (hym/git-delta-diff-visit-file) :type 'user-error)
        (should-not visited)))))

(ert-deftest hym/git-delta-visit-file-on-stat-line-does-not-open-file ()
  (let (visited)
    (hym/git-delta-test-with-find-file-stub visited
      (hym/git-delta-test-with-fixture
        (setq-local hym/git-delta-diff--directory "/repo/")
        (hym/git-delta-diff-visit-file)
        (should (looking-at "f1.txt"))
        (should-not visited)))))

(ert-deftest hym/git-delta-sticky-header-shows-file-and-hunk-above-position ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^│ 11 │k")
    (should (equal (substring-no-properties (hym/git-delta-diff--sticky-header-at (point)))
                   "f1.txt — 9: h"))))

(ert-deftest hym/git-delta-sticky-header-on-file-header-has-no-hunk ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (should (equal (substring-no-properties (hym/git-delta-diff--sticky-header-at (point)))
                   "added: f3.txt"))))

(ert-deftest hym/git-delta-sticky-header-before-first-file-is-nil ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^ 2 files changed")
    (should-not (hym/git-delta-diff--sticky-header-at (point)))))

(ert-deftest hym/git-delta-mode-sets-header-line ()
  (with-temp-buffer
    (hym/git-delta-diff-mode)
    (should header-line-format)))

(defun hym/git-delta-test-faces-at (pos)
  (ensure-list (get-text-property pos 'face)))

(ert-deftest hym/git-delta-file-header-band-covers-header-and-rule ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (should (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (point))))
    (should (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (line-end-position))))
    (forward-line 1)
    (should (looking-at "─"))
    (should (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (point))))
    (should (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (line-end-position))))
    (forward-line 1)
    (should-not (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (point))))))

(ert-deftest hym/git-delta-hunk-header-has-no-band ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^9: h")
    (should-not (memq 'hym/git-delta-file-header (hym/git-delta-test-faces-at (point))))))

(ert-deftest hym/git-delta-gap-before-subsequent-file-headers-only ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^added: f3.txt$")
    (forward-line -1)
    (should (eolp))
    (should (equal (get-text-property (point) 'line-height) hym/git-delta-diff-file-gap))
    (hym/git-delta-test-goto-line-matching "^f1.txt$")
    (forward-line -1)
    (should-not (get-text-property (point) 'line-height))))

(ert-deftest hym/git-delta-next-file-skips-hunks ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^f1.txt$")
    (hym/git-delta-diff-next-file)
    (should (looking-at "added: f3.txt"))
    (should-error (hym/git-delta-diff-next-file) :type 'user-error)
    (should (looking-at "added: f3.txt"))))

(ert-deftest hym/git-delta-previous-file-from-body-then-skips-hunks ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^│    │ +│  1 │new")
    (hym/git-delta-diff-previous-file)
    (should (looking-at "added: f3.txt"))
    (hym/git-delta-diff-previous-file)
    (should (looking-at "f1.txt"))
    (should-error (hym/git-delta-diff-previous-file) :type 'user-error)
    (should (looking-at "f1.txt"))))

(ert-deftest hym/git-delta-next-hunk-crosses-file-boundaries ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^f1.txt$")
    (hym/git-delta-diff-next-hunk)
    (should (looking-at "1: "))
    (hym/git-delta-diff-next-hunk)
    (should (looking-at "9: h"))
    (hym/git-delta-diff-next-hunk)
    (should (looking-at "1: "))
    (should (save-excursion (forward-line -3) (looking-at "added: f3.txt")))
    (should-error (hym/git-delta-diff-next-hunk) :type 'user-error)))

(ert-deftest hym/git-delta-previous-hunk ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^│    │ +│  1 │new")
    (hym/git-delta-diff-previous-hunk)
    (should (looking-at "1: "))
    (hym/git-delta-diff-previous-hunk)
    (should (looking-at "9: h"))))

(ert-deftest hym/git-delta-stat-path-forms ()
  (should (equal (hym/git-delta-diff--stat-path " lib/foo.ex | 4 ++--") "lib/foo.ex"))
  (should (equal (hym/git-delta-diff--stat-path " old.txt => new.txt | 0") "new.txt"))
  (should (equal (hym/git-delta-diff--stat-path " lib/{old => new}/f.ex | 2 +-") "lib/new/f.ex"))
  (should (equal (hym/git-delta-diff--stat-path " img.png | Bin 0 -> 12 bytes") "img.png"))
  (should-not (hym/git-delta-diff--stat-path " 2 files changed, 3 insertions(+)"))
  (should-not (hym/git-delta-diff--stat-path "│  1 │a | b       │  1 │a | b")))

(ert-deftest hym/git-delta-resolve-stat-path-handles-truncation ()
  (let ((paths '("f1.txt" "lib/very/long/path/foo.ex")))
    (should (equal (hym/git-delta-diff--resolve-stat-path "f1.txt" paths) "f1.txt"))
    (should (equal (hym/git-delta-diff--resolve-stat-path "...long/path/foo.ex" paths)
                   "lib/very/long/path/foo.ex"))
    (should-not (hym/git-delta-diff--resolve-stat-path "nope.ex" paths))))

(ert-deftest hym/git-delta-visit-file-on-stat-line-jumps-to-header ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^ f3.txt | 1 \\+")
    (hym/git-delta-diff-visit-file)
    (should (looking-at "added: f3.txt"))))

(ert-deftest hym/git-delta-visit-file-on-stat-summary-errors ()
  (hym/git-delta-test-with-fixture
    (hym/git-delta-test-goto-line-matching "^ 2 files changed")
    (should-error (hym/git-delta-diff-visit-file) :type 'user-error)))

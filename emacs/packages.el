;; -*- lexical-binding: t -*-

;; Package version management on top of straight.el.  Versions are pinned by
;; straight's own lockfile, which it already consults when cloning, so a fresh
;; machine reproduces the pinned state without any help from this file.  What
;; is added here is visibility: what is installed, what is available upstream,
;; and a way to move between them without restarting Emacs.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defgroup hym/packages nil
  "Inspect and update straight.el packages."
  :group 'tools)

(defcustom hym/packages-max-jobs 8
  "Maximum number of concurrent `git fetch' processes during a refresh."
  :type 'integer :group 'hym/packages)

(defcustom hym/packages-no-reload
  '("straight.el" "compat" "general.el" "evil" "exec-path-from-shell"
    "emacs-libvterm")
  "Local repository names that must never be reloaded in place.
Most are loaded before or beneath the rest of the configuration, so
re-loading their files would leave the session half-initialized.  vterm is
here for a different reason: its native module cannot be swapped out of a
running Emacs at all, so a restart is the only way to pick up a new one.
Updating any of these takes effect on the next restart instead."
  :type '(repeat string) :group 'hym/packages)

(defcustom hym/packages-post-build
  '(("emacs-libvterm" . hym/packages--compile-vterm-module))
  "Alist of local repository name to a function run after that repo rebuilds.
straight.el only byte-compiles elisp.  A package with a compiled component
needs its own build step re-run against the new sources, and nothing in
straight knows to do that."
  :type '(alist :key-type string :value-type function)
  :group 'hym/packages)

(defvar vterm-always-compile-module)

(defun hym/packages--compile-vterm-module ()
  "Rebuild vterm's native module against the newly checked-out sources.
Loading vterm with `vterm-always-compile-module' bound reuses vterm's own
build recipe rather than duplicating its cmake invocation here, which would
go stale the moment upstream changed it."
  (let ((vterm-always-compile-module t))
    (load (expand-file-name "vterm.el" (straight--build-dir "vterm")) nil t)))

(cl-defstruct (hym/packages-row (:constructor hym/packages-row-create)
                                (:copier nil))
  "One local repository and everything known about its versions."
  repo packages installed installed-desc pinned target target-desc kind
  ahead behind)

(defun hym/packages--status (row)
  "Return the availability status of ROW as a symbol.
One of `unknown' (not fetched yet), `up-to-date', `new-release',
`new-commits' or `past-release'.  A nil `behind' count means the count
could not be established, which does not rule out an update."
  (let ((installed (hym/packages-row-installed row))
        (target (hym/packages-row-target row))
        (behind (hym/packages-row-behind row)))
    (cond
     ((null target) 'unknown)
     ((equal installed target) 'up-to-date)
     ((and behind (zerop behind)) 'past-release)
     ((eq (hym/packages-row-kind row) 'release) 'new-release)
     (t 'new-commits))))

(defun hym/packages--updatable-p (row)
  "Return non-nil when ROW has an update available to move forward onto."
  (and (memq (hym/packages--status row) '(new-release new-commits)) t))

(defun hym/packages--pin-note (row)
  "Return a note about ROW disagreeing with the lockfile, or nil."
  (let ((pinned (hym/packages-row-pinned row)))
    (cond
     ((null pinned) "unpinned")
     ((not (equal pinned (hym/packages-row-installed row))) "drifted"))))

(defun hym/packages--status-label (row)
  "Return the Status column text for ROW."
  (let* ((behind (hym/packages-row-behind row))
         (note (hym/packages--pin-note row))
         (base (pcase (hym/packages--status row)
                 ('unknown "unknown")
                 ('up-to-date "up to date")
                 ('past-release (format "%s past release"
                                        (or (hym/packages-row-ahead row) "?")))
                 ('new-release "new release")
                 ('new-commits (if behind
                                   (format "%d new commit%s" behind
                                           (if (= behind 1) "" "s"))
                                 "new commits")))))
    (concat base (and note (concat ", " note)))))

(defun hym/packages--lockfile-read (path)
  "Read the lockfile at PATH and return its alist, or nil if PATH is absent."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (read (current-buffer)))))

(defun hym/packages--lockfile-write (alist path)
  "Write ALIST to PATH in straight.el's lockfile format.
Entries are sorted by repository name so that diffs stay readable."
  (let ((sorted (cl-sort (copy-sequence alist) #'string-lessp :key #'car)))
    (make-directory (file-name-directory path) 'parents)
    (with-temp-file path
      (insert (format "(%s)\n:epsilon\n"
                      (mapconcat (apply-partially #'format "%S")
                                 sorted "\n "))))))

(defun hym/packages--git (repo &rest args)
  "Run git with ARGS inside REPO, returning trimmed stdout or nil on failure."
  (let ((default-directory (straight--repos-dir repo)))
    (with-temp-buffer
      (when (zerop (apply #'call-process "git" nil t nil args))
        (let ((out (string-trim (buffer-string))))
          (unless (string-empty-p out) out))))))

(defun hym/packages--recipe (repo)
  "Return the straight.el recipe registered for REPO."
  (gethash repo straight--repo-cache))

(defun hym/packages--repo-remote (repo)
  "Return the name of REPO's primary git remote."
  (or (plist-get (hym/packages--recipe repo) :remote)
      straight-vc-git-default-remote-name))

(defun hym/packages--remote-ref (repo)
  "Return the remote-tracking ref REPO should be compared against.
Prefers the recipe's `:branch', then the remote's own default branch, then
whatever branch is checked out locally.  Candidates are tried in that order
and the first one that actually resolves wins: `refs/remotes/<remote>/HEAD'
goes stale when upstream renames its default branch, and still names the
dead branch long after the local checkout has moved to the new one."
  (let* ((remote (hym/packages--repo-remote repo))
         (branches
          (delq nil
                (list (plist-get (hym/packages--recipe repo) :branch)
                      (when-let* ((ref (hym/packages--git
                                       repo "symbolic-ref" "--short"
                                       (format "refs/remotes/%s/HEAD" remote))))
                        (string-remove-prefix (concat remote "/") ref))
                      (hym/packages--git repo "rev-parse" "--abbrev-ref"
                                         "HEAD")))))
    (seq-find (lambda (ref)
                (hym/packages--git repo "rev-parse" "--verify" "--quiet"
                                   (concat ref "^{commit}")))
              (mapcar (lambda (branch) (concat remote "/" branch)) branches))))

(defun hym/packages--select-target (tip tag tag-sha)
  "Return (TARGET-SHA DESC KIND) given a branch TIP and its nearest TAG.
TAG-SHA is the commit TAG dereferences to.  A repo only counts as tracking
releases when both a tag and its commit resolved; otherwise it tracks the
branch tip."
  (if (and tag tag-sha)
      (list tag-sha tag 'release)
    (list tip (and tip (substring tip 0 (min 7 (length tip)))) 'commit)))

(defun hym/packages--parse-counts (output)
  "Parse OUTPUT of `git rev-list --left-right --count' into (AHEAD . BEHIND).
Returns (nil . nil) when OUTPUT is missing or malformed, which happens on
shallow clones where the two refs share no reachable ancestor."
  (if (and output (string-match "\\`\\([0-9]+\\)[ \t]+\\([0-9]+\\)" output))
      (cons (string-to-number (match-string 1 output))
            (string-to-number (match-string 2 output)))
    (cons nil nil)))

(defun hym/packages--repos ()
  "Return the sorted names of all local repositories straight.el knows about."
  (let (repos)
    (maphash (lambda (repo _recipe) (push repo repos)) straight--repo-cache)
    (sort repos #'string-lessp)))

(defun hym/packages--repo-packages (repo)
  "Return the names of packages built from REPO."
  (let (packages)
    (maphash (lambda (package recipe)
               (when (equal repo (plist-get recipe :local-repo))
                 (push package packages)))
             straight--recipe-cache)
    (sort packages #'string-lessp)))

(defun hym/packages--initial-row (repo pinned)
  "Build the row for REPO from local state only.
PINNED is the lockfile alist.  Target fields stay nil until a fetch has
happened, which is what makes the buffer appear instantly."
  (hym/packages-row-create
   :repo repo
   :packages (hym/packages--repo-packages repo)
   :installed (hym/packages--git repo "rev-parse" "HEAD")
   :installed-desc (or (hym/packages--git repo "describe" "--tags" "--always"
                                          "HEAD")
                       "?")
   :pinned (cdr (assoc repo pinned))
   :target nil :target-desc nil :kind 'commit :ahead nil :behind nil))

(defun hym/packages--fill-target (row)
  "Fill ROW's target fields from remote refs already present locally.
Assumes a fetch has completed; performs no network access itself."
  (let* ((repo (hym/packages-row-repo row))
         (ref (hym/packages--remote-ref repo))
         (tip (and ref (hym/packages--git repo "rev-parse" ref)))
         (tag (and tip (hym/packages--git repo "describe" "--tags"
                                          "--abbrev=0" tip)))
         ;; Annotated tags point at a tag object, not a commit; ^{} peels it.
         (tag-sha (and tag (hym/packages--git repo "rev-parse"
                                              (concat tag "^{}")))))
    (cl-destructuring-bind (target desc kind)
        (hym/packages--select-target tip tag tag-sha)
      (setf (hym/packages-row-target row) target
            (hym/packages-row-target-desc row) desc
            (hym/packages-row-kind row) kind)
      (let ((counts (hym/packages--parse-counts
                     (and target
                          (hym/packages--git
                           repo "rev-list" "--left-right" "--count"
                           (format "%s...%s"
                                   (hym/packages-row-installed row) target))))))
        (setf (hym/packages-row-ahead row) (car counts)
              (hym/packages-row-behind row) (cdr counts))))
    row))

(defun hym/packages--run-queue (jobs max)
  "Run JOBS with at most MAX of them in flight at once.
Each job is a function of one argument: a callback it must invoke exactly
once when its work has finished, which admits the next queued job."
  (let ((pending jobs)
        (active 0))
    (letrec ((pump
              (lambda ()
                (while (and pending (< active max))
                  (let ((job (pop pending)))
                    (cl-incf active)
                    (funcall job (lambda ()
                                   (cl-decf active)
                                   (funcall pump))))))))
      (funcall pump))
    nil))

(defvar hym/packages--config-map nil
  "Cached alist of package name to the config file declaring it.")

(defun hym/packages--loaded-files (dir)
  "Return the files under DIR that appear in `load-history'.
Re-loading only these refreshes what is actually live in the session,
rather than pulling in files the package never loaded."
  (let ((prefix (file-name-as-directory (expand-file-name dir))))
    (seq-filter (lambda (file)
                  (and (stringp file) (string-prefix-p prefix file)))
                (mapcar #'car load-history))))

(defun hym/packages--scan-config (dir)
  "Return an alist mapping package name to the file in DIR declaring it.
Only top-level `use-package' forms count, so commented-out and nested
mentions are ignored."
  (let (map)
    (dolist (file (directory-files dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(use-package[ \t\n]+\\([^ \t\n()]+\\)" nil t)
          (push (cons (match-string 1) file) map))))
    (nreverse map)))

(defun hym/packages--config-map ()
  "Return the package-to-config-file map, scanning on first use."
  (or hym/packages--config-map
      (setq hym/packages--config-map
            (hym/packages--scan-config user-emacs-directory))))

(defun hym/packages--config-files (row)
  "Return the config files declaring any of ROW's packages, without duplicates."
  (let ((map (hym/packages--config-map)))
    (seq-uniq (delq nil (mapcar (lambda (package) (cdr (assoc package map)))
                                (hym/packages-row-packages row))))))

(defun hym/packages--reloadable-p (row)
  "Return non-nil unless ROW's repo is in `hym/packages-no-reload'."
  (not (member (hym/packages-row-repo row) hym/packages-no-reload)))

(defun hym/packages--reload (row)
  "Reload ROW's packages and re-evaluate the config that sets them up.
Returns non-nil when a reload happened."
  (when (hym/packages--reloadable-p row)
    (dolist (package (hym/packages-row-packages row))
      (dolist (file (hym/packages--loaded-files (straight--build-dir package)))
        (load (file-name-sans-extension file) nil 'nomessage)))
    (dolist (file (hym/packages--config-files row))
      (load file nil 'nomessage))
    t))

(defvar-local hym/packages--rows nil
  "List of `hym/packages-row' structs backing the current buffer.")

(defvar-local hym/packages--marked nil
  "Repo names currently marked for update in this buffer.")

(defvar-local hym/packages--progress nil
  "Cons of (COMPLETED . TOTAL) for an in-flight refresh, or nil.")

(defun hym/packages--make-process (&rest args)
  "Call `make-process' with ARGS.
Kept as a wrapper so asynchronous refresh can be tested without spawning
real subprocesses."
  (apply #'make-process args))

(defun hym/packages--lockfile-path ()
  "Return the path of the lockfile for straight.el's default profile."
  (straight--versions-file (alist-get nil straight-profiles)))

(defun hym/packages--build-rows ()
  "Return a freshly built row for every known local repository."
  (let ((pinned (hym/packages--lockfile-read (hym/packages--lockfile-path))))
    (mapcar (lambda (repo) (hym/packages--initial-row repo pinned))
            (hym/packages--repos))))

(defun hym/packages--entry (row)
  "Return the `tabulated-list-entries' element for ROW."
  (list (hym/packages-row-repo row)
        (vector (hym/packages-row-repo row)
                (or (hym/packages-row-installed-desc row) "?")
                (or (hym/packages-row-target-desc row) "")
                (hym/packages--status-label row))))

(defun hym/packages--sorted-rows ()
  "Return `hym/packages--rows' with updatable repositories first."
  (let ((rows (copy-sequence hym/packages--rows)))
    (sort rows (lambda (a b)
                 (let ((ua (hym/packages--updatable-p a))
                       (ub (hym/packages--updatable-p b)))
                   (if (eq ua ub)
                       (string-lessp (hym/packages-row-repo a)
                                     (hym/packages-row-repo b))
                     ua))))))

(defun hym/packages--render ()
  "Rebuild and reprint the list, preserving point and marks."
  (setq tabulated-list-entries
        (mapcar #'hym/packages--entry (hym/packages--sorted-rows)))
  (tabulated-list-print t t)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag
       (if (member (tabulated-list-get-id) hym/packages--marked) "*" " ")
       t))))

(defun hym/packages--row-at-point ()
  "Return the row under point, or nil."
  (when-let* ((repo (tabulated-list-get-id)))
    (seq-find (lambda (row) (equal repo (hym/packages-row-repo row)))
              hym/packages--rows)))

(defun hym/packages--marked-rows ()
  "Return the rows currently marked, in list order."
  (seq-filter (lambda (row)
                (member (hym/packages-row-repo row) hym/packages--marked))
              hym/packages--rows))

(defun hym/packages--update-mode-line ()
  "Show refresh progress, if any, in the mode line."
  (setq mode-line-process
        (when hym/packages--progress
          (format " [%d/%d]" (car hym/packages--progress)
                  (cdr hym/packages--progress))))
  (force-mode-line-update))

(defun hym/packages--fetch-job (buffer row)
  "Return a queue job fetching ROW's repo and updating it in BUFFER."
  (lambda (done)
    (let* ((repo (hym/packages-row-repo row))
           (finish
            (lambda ()
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (hym/packages--fill-target row)
                  (cl-incf (car hym/packages--progress))
                  (when (= (car hym/packages--progress)
                           (cdr hym/packages--progress))
                    (setq hym/packages--progress nil))
                  (hym/packages--update-mode-line)
                  (hym/packages--render)))
              (funcall done))))
      (condition-case err
          (hym/packages--make-process
           :name (format "hym/packages-fetch-%s" repo)
           :buffer nil
           :noquery t
           :command (list "git" "-C" (straight--repos-dir repo)
                          "fetch" "--tags" "--quiet"
                          (hym/packages--repo-remote repo))
           ;; A sentinel can fire more than once; only the exit must count, or
           ;; the queue would over-admit and the progress count overshoot.
           :sentinel (lambda (process _event)
                       (unless (process-live-p process) (funcall finish))))
        ;; A repo with no remote, or a missing git, must not stall the queue.
        (error (message "hym/packages: fetch failed for %s: %s"
                        repo (error-message-string err))
               (funcall finish))))))

(defun hym/packages-refresh ()
  "Fetch every repository and update the list as results arrive."
  (interactive)
  (when hym/packages--progress
    (user-error "Refresh already running (%d/%d)"
                (car hym/packages--progress) (cdr hym/packages--progress)))
  (let ((rows hym/packages--rows)
        (buffer (current-buffer)))
    (setq hym/packages--progress (cons 0 (length rows)))
    (hym/packages--update-mode-line)
    (hym/packages--run-queue
     (mapcar (lambda (row) (hym/packages--fetch-job buffer row)) rows)
     hym/packages-max-jobs)))

(defun hym/packages-mark ()
  "Mark the repository at point for update and move to the next line."
  (interactive)
  (if-let* ((row (hym/packages--row-at-point)))
      (if (hym/packages--updatable-p row)
          (progn
            (cl-pushnew (hym/packages-row-repo row) hym/packages--marked
                        :test #'equal)
            (tabulated-list-put-tag "*" t))
        (message "No update available for %s" (hym/packages-row-repo row))
        (forward-line 1))
    (forward-line 1)))

(defun hym/packages-unmark ()
  "Unmark the repository at point and move to the next line."
  (interactive)
  (when-let* ((repo (tabulated-list-get-id)))
    (setq hym/packages--marked (delete repo hym/packages--marked)))
  (tabulated-list-put-tag " " t))

(defun hym/packages-mark-updatable ()
  "Mark every repository with an available update."
  (interactive)
  (setq hym/packages--marked
        (mapcar #'hym/packages-row-repo
                (seq-filter #'hym/packages--updatable-p hym/packages--rows)))
  (hym/packages--render)
  (message "%d package(s) marked" (length hym/packages--marked)))

(defun hym/packages-quit ()
  "Quit the package list."
  (interactive)
  (quit-window))

(defun hym/packages--lockfile-update (updates path)
  "Merge UPDATES, an alist of repo to commit, into the lockfile at PATH."
  (let ((alist (hym/packages--lockfile-read path)))
    (pcase-dolist (`(,repo . ,commit) updates)
      (if-let* ((cell (assoc repo alist)))
          (setcdr cell commit)
        (push (cons repo commit) alist)))
    (hym/packages--lockfile-write alist path)))

(defun hym/packages--reload-safely (row)
  "Reload ROW, reporting rather than signaling if it fails.
Loading a package runs arbitrary code, so a reload can fail for reasons that
have nothing to do with the update having worked.  One package failing that
way must not abandon the rest of a batch."
  (condition-case err
      (and (hym/packages--reload row) t)
    (error (message "hym/packages: %s updated but failed to reload: %s"
                    (hym/packages-row-repo row) (error-message-string err))
           nil)))

(defun hym/packages--apply-row (row)
  "Check out, rebuild and reload ROW's target.  Return a result plist.
The lockfile entry is written the instant the checkout lands, before anything
that could fail, so a package can never end up checked out at a commit the
lockfile does not record."
  (let ((repo (hym/packages-row-repo row))
        (target (hym/packages-row-target row)))
    (straight-vc-check-out-commit (hym/packages--recipe repo) target)
    (hym/packages--lockfile-update (list (cons repo target))
                                   (hym/packages--lockfile-path))
    (dolist (package (hym/packages-row-packages row))
      ;; straight-check-for-modifications is nil, so straight neither notices
      ;; the checkout nor re-symlinks the build directory -- meaning a file
      ;; added upstream never appears in it and requiring it fails.  Removing
      ;; the directory forces a real rebuild rather than a byte-compile over a
      ;; stale file list.
      (let ((dir (straight--build-dir package)))
        (when (file-directory-p dir)
          (delete-directory dir t)))
      (straight-rebuild-package package))
    (when-let* ((post (alist-get repo hym/packages-post-build nil nil #'equal)))
      (funcall post))
    (list :repo repo
          :desc (hym/packages-row-target-desc row)
          :reloaded (hym/packages--reload-safely row))))

(defun hym/packages--resync-row (row)
  "Re-read ROW's local state and re-derive its target, without fetching.
Used after an update so the row reflects the new checkout while the rest of
the list keeps the upstream data an earlier refresh paid for."
  (let ((repo (hym/packages-row-repo row))
        (pinned (hym/packages--lockfile-read (hym/packages--lockfile-path))))
    (setf (hym/packages-row-installed row)
          (hym/packages--git repo "rev-parse" "HEAD")
          (hym/packages-row-installed-desc row)
          (or (hym/packages--git repo "describe" "--tags" "--always" "HEAD") "?")
          (hym/packages-row-pinned row) (cdr (assoc repo pinned)))
    (hym/packages--fill-target row)))

(defun hym/packages--summary (results)
  "Return a message describing RESULTS, a list of apply-row plists."
  (if (null results)
      "Nothing to update."
    (let* ((failed (seq-filter (lambda (r) (plist-get r :error)) results))
           (ok (seq-remove (lambda (r) (plist-get r :error)) results))
           (stale (mapcar (lambda (r) (plist-get r :repo))
                          (seq-remove (lambda (r) (plist-get r :reloaded)) ok))))
      (string-join
       (delq nil
             (list
              (when ok
                (format "Updated %s."
                        (string-join
                         (mapcar (lambda (r) (format "%s %s" (plist-get r :repo)
                                                     (plist-get r :desc)))
                                 ok)
                         ", ")))
              (when stale
                (format "Restart needed for: %s." (string-join stale ", ")))
              (when failed
                (format "Failed: %s."
                        (string-join
                         (mapcar (lambda (r) (format "%s (%s)" (plist-get r :repo)
                                                     (plist-get r :error)))
                                 failed)
                         ", ")))))
       " "))))

(defun hym/packages-execute ()
  "Apply the updates for every marked package."
  (interactive)
  (let ((rows (hym/packages--marked-rows)))
    (if (null rows)
        (message "No packages marked")
      (when (yes-or-no-p (format "Update %d package(s)? " (length rows)))
        (let (results)
          (dolist (row rows)
            (let ((repo (hym/packages-row-repo row)))
              (message "hym/packages: updating %s..." repo)
              ;; Sequential on purpose: concurrent rebuilds would interleave.
              ;; Isolated per package so one failure cannot abandon the rest of
              ;; the batch part-way, which would leave everything after it
              ;; untouched and everything before it unreported.
              (push (condition-case err
                        (hym/packages--apply-row row)
                      (error (list :repo repo
                                   :desc (hym/packages-row-target-desc row)
                                   :error (error-message-string err))))
                    results)))
          (setq results (nreverse results))
          (setq hym/packages--marked nil)
          (mapc #'hym/packages--resync-row rows)
          (hym/packages--render)
          (message "%s" (hym/packages--summary results)))))))

(defun hym/packages-show-log ()
  "Show the commits between the installed and available versions at point."
  (interactive)
  (let* ((row (or (hym/packages--row-at-point) (user-error "No package here")))
         (repo (hym/packages-row-repo row))
         (installed (hym/packages-row-installed row))
         (target (or (hym/packages-row-target row)
                     (user-error "Refresh with `g' first")))
         (log (hym/packages--git repo "log" "--oneline" "--no-decorate"
                                 (format "%s..%s" installed target))))
    (if (null log)
        (message "No commits between %s and %s"
                 (hym/packages-row-installed-desc row)
                 (hym/packages-row-target-desc row))
      (with-current-buffer (get-buffer-create "*hym/packages-log*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "%s: %s..%s\n\n" repo
                          (hym/packages-row-installed-desc row)
                          (hym/packages-row-target-desc row))
                  log "\n")
          (goto-char (point-min)))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

(defun hym/packages-checkout (ref)
  "Check out REF in the repository at point, then rebuild and reload it.
This is how a package sitting past its last release is moved deliberately
onto a tag, in either direction."
  (interactive
   (let* ((row (or (hym/packages--row-at-point) (user-error "No package here")))
          (repo (hym/packages-row-repo row))
          (tags (split-string (or (hym/packages--git
                                   repo "tag" "--sort=-creatordate")
                                  "")
                              "\n" t)))
     (list (completing-read (format "Check out in %s: " repo) tags))))
  (let* ((row (hym/packages--row-at-point))
         (repo (hym/packages-row-repo row))
         (sha (or (hym/packages--git repo "rev-parse" (concat ref "^{commit}"))
                  (user-error "No such ref in %s: %s" repo ref))))
    (setf (hym/packages-row-target row) sha
          (hym/packages-row-target-desc row) ref)
    (let ((result (hym/packages--apply-row row)))
      (hym/packages--resync-row row)
      (hym/packages--render)
      (message "%s" (hym/packages--summary (list result))))))

(defun hym/packages-freeze ()
  "Rewrite the whole lockfile from what is currently checked out.
Use this after changing package versions outside this buffer."
  (interactive)
  (straight-freeze-versions t)
  ;; Freezing moves no checkout, so only the pinned column can have changed.
  (let ((pinned (hym/packages--lockfile-read (hym/packages--lockfile-path))))
    (dolist (row hym/packages--rows)
      (setf (hym/packages-row-pinned row)
            (cdr (assoc (hym/packages-row-repo row) pinned)))))
  (hym/packages--render)
  (message "Lockfile rewritten from installed versions"))

(defvar hym/packages-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g")   #'hym/packages-refresh)
    (define-key map (kbd "m")   #'hym/packages-mark)
    (define-key map (kbd "u")   #'hym/packages-unmark)
    (define-key map (kbd "U")   #'hym/packages-mark-updatable)
    (define-key map (kbd "x")   #'hym/packages-execute)
    (define-key map (kbd "RET") #'hym/packages-show-log)
    (define-key map (kbd "c")   #'hym/packages-checkout)
    (define-key map (kbd "F")   #'hym/packages-freeze)
    (define-key map (kbd "q")   #'hym/packages-quit)
    map)
  "Keymap for `hym/packages-mode'.")

(define-derived-mode hym/packages-mode tabulated-list-mode "Packages"
  "Major mode for listing straight.el packages and available updates.

  g     refresh: fetch every repository
  m     mark the package at point for update
  u     unmark
  U     mark every package with an available update
  x     apply updates for all marked packages
  RET   show the commits an update would bring in
  c     check out a specific tag or ref
  F     rewrite the lockfile from what is installed
  q     quit"
  (setq tabulated-list-format
        [("Package"   28 t)
         ("Installed" 24 t)
         ("Available" 16 t)
         ("Status"    26 t)])
  (setq tabulated-list-padding 2)
  (setq truncate-lines t)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  ;; Evil owns m, u, U and g in normal state, so bind them locally as well.
  (when (fboundp 'evil-normal-state)
    (evil-normal-state 1))
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "m") #'hym/packages-mark)
    (evil-local-set-key 'normal (kbd "u") #'hym/packages-unmark)
    (evil-local-set-key 'normal (kbd "U") #'hym/packages-mark-updatable)
    (evil-local-set-key 'normal (kbd "g") #'hym/packages-refresh)
    (evil-local-set-key 'normal (kbd "x")   #'hym/packages-execute)
    (evil-local-set-key 'normal (kbd "RET") #'hym/packages-show-log)
    (evil-local-set-key 'normal (kbd "c")   #'hym/packages-checkout)
    (evil-local-set-key 'normal (kbd "F")   #'hym/packages-freeze)
    (evil-local-set-key 'normal (kbd "q")   #'hym/packages-quit)))

;;;###autoload
(defun hym/packages ()
  "List installed packages, their versions, and available updates.
The list appears immediately from local state and a fetch starts straight
away, so upstream versions fill in without waiting for a keypress.  A fetch
already in flight is left alone: rebuilding the rows underneath it would
orphan the objects its sentinels are holding, and the display would quietly
stop filling in."
  (interactive)
  (let ((buffer (get-buffer-create "*packages*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'hym/packages-mode)
        (hym/packages-mode))
      (unless hym/packages--progress
        (setq hym/packages--rows (hym/packages--build-rows))
        (setq hym/packages--marked nil)
        (hym/packages--render)
        (hym/packages-refresh)))
    (pop-to-buffer buffer)))

(provide 'packages)

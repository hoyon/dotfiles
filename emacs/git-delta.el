;; -*- lexical-binding: t -*-

;; Use delta to show side by side diffs of staged and unstaged changes

(defvar-local hym/git-delta-diff--directory nil)
(defvar-local hym/git-delta-diff--command-fn nil)
(defvar-local hym/git-delta-diff--rendered-width nil
  "Column width the current delta buffer was last rendered at.")

(defun hym/git-delta-diff--width ()
  "Columns to render delta into.
The diff window's text width, so a side window (e.g. the workspace
sidebar) taking part of the frame is accounted for; falls back to the
frame width when the buffer is not displayed yet."
  (if-let ((win (get-buffer-window (current-buffer) t)))
      (window-body-width win)
    (- (frame-width) 1)))

(defun hym/git-delta-diff-refresh ()
  "Refresh the current delta diff buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (default-directory hym/git-delta-diff--directory)
        (pos (point)))
    (erase-buffer)
    (call-process-shell-command (funcall hym/git-delta-diff--command-fn) nil t)
    (if (= (point-min) (point-max))
        (insert "No changes")
      (ansi-color-apply-on-region (point-min) (point-max)))
    (setq hym/git-delta-diff--rendered-width (hym/git-delta-diff--width))
    (goto-char (min pos (point-max)))))

(defun hym/git-delta-diff--show-buffer (buf)
  "Display BUF in a tab, reusing existing tab if present."
  (if-let ((tab (tab-bar-get-buffer-tab buf t t)))
      (progn
        (tab-bar-switch-to-tab (alist-get 'name tab))
        (switch-to-buffer buf))
    (switch-to-buffer-other-tab buf)
    (tab-bar-rename-tab "delta")))

(defun hym/git-delta-diff-buffer (args buf-name command-fn)
  "Set up a delta diff buffer and return it, WITHOUT running delta.
The caller should display it and then call `hym/git-delta-diff-refresh',
so delta runs once at the displayed window's width rather than the whole
frame's. ARGS, BUF-NAME and COMMAND-FN are as described in
`hym/git-delta-diff'."
  (let* ((dir (magit-toplevel))
         (default-directory dir)
         (diff-type (or buf-name
                        (if (string= args "--cached") "staged" "unstaged")))
         (buf (get-buffer-create (format "*delta-diff[%s]: %s*" diff-type (project-name (project-current))))))
    (with-current-buffer buf
      (special-mode)
      (setq-local hym/git-delta-diff--directory dir)
      (setq-local hym/git-delta-diff--command-fn
                  (or command-fn
                      (lambda ()
                        (format "{ GIT_PAGER=cat git diff --stat %1$s; echo; GIT_PAGER=cat git diff -U5 %1$s | delta --side-by-side --width %2$d; }"
                                (or args "")
                                (hym/git-delta-diff--width)))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Loading diff…")
        (goto-char (point-min)))
      (evil-local-set-key 'normal "q" 'tab-close)
      (evil-local-set-key 'normal "gr" 'hym/git-delta-diff-refresh))
    buf))

(defun hym/git-delta-diff (&optional args buf-name command-fn)
  "Show git diff through delta side-by-side in a buffer.
ARGS are passed to git diff. BUF-NAME overrides the buffer name.
COMMAND-FN, if provided, is a function returning the shell command to run."
  (interactive)
  (unless (executable-find "delta")
    (user-error "delta not found in PATH"))
  (let ((buf (hym/git-delta-diff-buffer args buf-name command-fn)))
    (hym/git-delta-diff--show-buffer buf)
    ;; Re-render now that BUF is in a window, so the width matches the
    ;; window rather than the whole frame.
    (with-current-buffer buf (hym/git-delta-diff-refresh))))

(defun hym/git-delta-diff-staged ()
  "Show staged diff through delta side-by-side."
  (interactive)
  (hym/git-delta-diff "--cached"))

(defun hym/git-delta-diff-unstaged ()
  "Show unstaged diff through delta side-by-side."
  (interactive)
  (hym/git-delta-diff))

(defun hym/git-delta-diff-unstaged-with-untracked ()
  "Show unstaged diff, including untracked files, through delta side-by-side."
  (interactive)
  (hym/git-delta-diff
   nil "unstaged+untracked"
   (lambda ()
     (format "{ GIT_PAGER=cat git diff --stat; git ls-files --others --exclude-standard | while IFS= read -r f; do GIT_PAGER=cat git diff --stat --no-index /dev/null \"$f\"; done; echo; { GIT_PAGER=cat git diff -U5; git ls-files --others --exclude-standard | while IFS= read -r f; do GIT_PAGER=cat git diff --no-index /dev/null \"$f\"; done; } | delta --side-by-side --width %d; }"
             (hym/git-delta-diff--width)))))

(defun hym/git-delta-diff-merge-base ()
  "Show delta diff from merge base with default branch."
  (interactive)
  (let* ((default-directory (magit-toplevel))
         (default-branch (magit-main-branch))
         (merge-base (magit-git-string "merge-base" default-branch "HEAD")))
    (message merge-base)
    (hym/git-delta-diff (format "%s..HEAD" merge-base) "merge-base")))

(defun hym/git-delta-diff--section-type-p (type)
  "Return non-nil if current section or its parent has TYPE."
  (when-let ((section (magit-current-section)))
    (or (eq (oref section type) type)
        (and-let* ((parent (oref section parent)))
          (eq (oref parent type) type)))))

(defun hym/git-delta-diff--selected-commit-range ()
  "Return a git diff range covering the selected commits."
  (when-let ((commits (magit-region-values 'commit t)))
    (when (cdr commits)
      (deactivate-mark)
      (format "%s^..%s" (car (last commits)) (car commits)))))

(defun hym/git-delta-diff-dwim ()
  "Show delta diff based on context in Magit buffer."
  (interactive)
  (let* ((section (magit-current-section))
         (file (or (magit-file-at-point)
                   (and section (oref section value))))
         (commit-range (hym/git-delta-diff--selected-commit-range))
         (commit (magit-commit-at-point))
         (in-staged (hym/git-delta-diff--section-type-p 'staged))
         (in-untracked (hym/git-delta-diff--section-type-p 'untracked)))
    (cond
     (commit-range
      (hym/git-delta-diff commit-range "selected commits"))
     (commit
      (hym/git-delta-diff (format "%s^..%s" commit commit)))
     ((and file in-staged)
      (hym/git-delta-diff (format "--cached -- %s" (shell-quote-argument file))))
     ((and file in-untracked)
      (let ((qf (shell-quote-argument file)))
        (hym/git-delta-diff
         nil (format "untracked: %s" file)
         (lambda () (format "git diff --no-index /dev/null %s | delta --side-by-side --width %d"
                            qf (hym/git-delta-diff--width))))))
     (file
      (hym/git-delta-diff (format "-- %s" (shell-quote-argument file))))
     (in-staged
      (hym/git-delta-diff-staged))
     (t
      (hym/git-delta-diff-unstaged)))))

(defvar hym/git-delta-diff--resize-timer nil)

(defun hym/git-delta-diff--window-stale-p (win)
  "Non-nil if WIN shows a delta buffer rendered at a different width."
  (let ((buf (window-buffer win)))
    (and (buffer-local-value 'hym/git-delta-diff--command-fn buf)
         (not (equal (buffer-local-value 'hym/git-delta-diff--rendered-width buf)
                     (window-body-width win))))))

(defun hym/git-delta-diff--refresh-visible (frame)
  "Re-render delta buffers in FRAME whose window width changed."
  (when (frame-live-p frame)
    (dolist (win (window-list frame))
      (when (hym/git-delta-diff--window-stale-p win)
        (with-current-buffer (window-buffer win)
          (hym/git-delta-diff-refresh))))))

(defun hym/git-delta-diff--on-resize (frame)
  "Debounced re-render when a delta buffer's window width changes.
Keyed on the window width, not the frame, so toggling a side window
\(e.g. the workspace sidebar) re-renders at the new width too."
  (when (and (frame-live-p frame)
             (seq-some #'hym/git-delta-diff--window-stale-p (window-list frame)))
    (when (timerp hym/git-delta-diff--resize-timer)
      (cancel-timer hym/git-delta-diff--resize-timer))
    (setq hym/git-delta-diff--resize-timer
          (run-at-time 0.3 nil #'hym/git-delta-diff--refresh-visible frame))))

(add-hook 'window-size-change-functions #'hym/git-delta-diff--on-resize)

(hym/leader-def
  "gd" 'hym/git-delta-diff-staged
  "gD" 'hym/git-delta-diff-unstaged
  "gf" 'hym/git-delta-diff-merge-base)

(general-define-key
 :keymaps '(magit-status-mode-map magit-log-mode-map)
 "D" 'hym/git-delta-diff-dwim)

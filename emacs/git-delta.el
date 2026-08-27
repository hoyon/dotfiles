;; -*- lexical-binding: t -*-

;; Use delta to show side by side diffs of staged and unstaged changes

(require 'ansi-color)
(require 'outline)
(require 'text-property-search)

(defvar-local hym/git-delta-diff--directory nil)
(defvar-local hym/git-delta-diff--command-fn nil)
(defvar-local hym/git-delta-diff--rendered-width nil
  "Column width the current delta buffer was last rendered at.")

(defun hym/git-delta-diff--parse-file-header (line)
  (cond
   ((string-match "\\`renamed: .* ⟶ +\\(.+\\)\\'" line)
    (cons (match-string 1 line) 'renamed))
   ((string-match "\\`added: \\(.+\\)\\'" line)
    (cons (match-string 1 line) 'added))
   ((string-match "\\`removed: \\(.+\\)\\'" line)
    (cons (match-string 1 line) 'removed))
   (t (cons line 'modified))))

(defun hym/git-delta-diff--hunk-line-number (line)
  (when (string-match "\\`\\([0-9]+\\): " line)
    (string-to-number (match-string 1 line))))

(defun hym/git-delta-diff--right-line-number (line)
  (let ((cells (split-string line "│")))
    (when (> (length cells) 3)
      (let ((cell (string-trim (nth 3 cells))))
        (when (string-match-p "\\`[0-9]+\\'" cell)
          (string-to-number cell))))))

(defun hym/git-delta-diff--file-header-p (line)
  (and (not (string-empty-p line))
       (not (string-match-p "\\`[│─[:space:]]" line))
       (save-excursion
         (and (zerop (forward-line 1))
              (eq (char-after) ?─)))))

(defface hym/git-delta-file-header
  '((t :inherit (header-line bold) :extend t))
  "Band behind a file header and its rule in delta diff buffers.")

(defvar hym/git-delta-diff-file-gap 3.0
  "Height, in lines, of the blank line separating one file's diff from the next.")

(defun hym/git-delta-diff--decorate-file-header (bol first)
  (save-excursion
    (goto-char bol)
    (forward-line 1)
    (add-face-text-property bol (min (point-max) (1+ (line-end-position)))
                            'hym/git-delta-file-header)
    (goto-char bol)
    (when (and (not first) (zerop (forward-line -1)) (eolp))
      (put-text-property (point) (1+ (point)) 'line-height hym/git-delta-diff-file-gap))))

(defun hym/git-delta-diff--mark-headings ()
  (save-excursion
    (goto-char (point-min))
    (let ((first-file t))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (hunk (hym/git-delta-diff--hunk-line-number line)))
          (cond
           (hunk
            (add-text-properties bol eol `(hym/git-delta-heading 2 hym/git-delta-hunk ,hunk)))
           ((hym/git-delta-diff--file-header-p line)
            (add-text-properties
             bol eol
             `(hym/git-delta-heading 1
               hym/git-delta-file ,(hym/git-delta-diff--parse-file-header line)))
            (hym/git-delta-diff--decorate-file-header bol first-file)
            (setq first-file nil))))
        (forward-line 1)))))

(defun hym/git-delta-diff--imenu-index ()
  (let (index match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'hym/git-delta-file))
        (push (cons (car (prop-match-value match))
                    (copy-marker (prop-match-beginning match)))
              index)))
    (nreverse index)))

(defun hym/git-delta-diff--location-at-point ()
  (save-excursion
    (beginning-of-line)
    (let (file line)
      (while (and (not file) (not (bobp)))
        (let ((bol (point)))
          (setq file (get-text-property bol 'hym/git-delta-file))
          (unless (or file line)
            (setq line (or (get-text-property bol 'hym/git-delta-hunk)
                           (hym/git-delta-diff--right-line-number
                            (buffer-substring-no-properties bol (line-end-position))))))
          (forward-line -1)))
      (unless file
        (setq file (get-text-property (point) 'hym/git-delta-file)))
      (when file
        (list (car file) (cdr file) (or line 1))))))

(defun hym/git-delta-diff--outline-search (&optional bound move backward looking-at)
  (outline-search-text-property 'hym/git-delta-heading nil bound move backward looking-at))

(defun hym/git-delta-diff--outline-level ()
  (get-text-property (point) 'hym/git-delta-heading))

(defun hym/git-delta-diff--headings ()
  (let (headings path match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'hym/git-delta-heading))
        (let* ((pos (prop-match-beginning match))
               (file (get-text-property pos 'hym/git-delta-file))
               (hunk (get-text-property pos 'hym/git-delta-hunk)))
          (when file (setq path (car file)))
          (push (cons (if hunk (cons path hunk) path) pos) headings))))
    (nreverse headings)))

(defun hym/git-delta-diff--collapsed-headings ()
  (save-excursion
    (seq-keep (lambda (heading)
                (goto-char (cdr heading))
                (and (outline-invisible-p (line-end-position))
                     (car heading)))
              (hym/git-delta-diff--headings))))

(defun hym/git-delta-diff--restore-folds (keys)
  (let ((headings (hym/git-delta-diff--headings)))
    (save-excursion
      (dolist (key keys)
        (when-let ((pos (cdr (assoc key headings))))
          (goto-char pos)
          (outline-hide-subtree))))))

(defun hym/git-delta-diff--stat-path (line)
  (when (string-match "\\` \\(.+?\\) +| " line)
    (let ((path (replace-regexp-in-string "{[^}]* => \\([^}]*\\)}" "\\1"
                                          (match-string 1 line))))
      (if (string-match "\\`.* => \\(.+\\)\\'" path)
          (match-string 1 path)
        path))))

(defun hym/git-delta-diff--resolve-stat-path (stat-path paths)
  (if (string-prefix-p "..." stat-path)
      (let ((suffix (substring stat-path 3)))
        (seq-find (lambda (path) (string-suffix-p suffix path)) paths))
    (car (member stat-path paths))))

(defun hym/git-delta-diff--goto-stat-file ()
  (let* ((index (hym/git-delta-diff--imenu-index))
         (stat-path (hym/git-delta-diff--stat-path
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position))))
         (path (and stat-path
                    (hym/git-delta-diff--resolve-stat-path stat-path (mapcar #'car index)))))
    (unless path
      (user-error "No file at point"))
    (goto-char (cdr (assoc path index)))
    (hym/git-delta-diff--scroll-to-top)))

(defun hym/git-delta-diff--scroll-to-top ()
  (when (eq (window-buffer) (current-buffer))
    (recenter 0)))

(defun hym/git-delta-diff-visit-file ()
  "Open the file under point at the line shown in the right-hand pane.
On a line of the stat block at the top, jump to that file's header instead."
  (interactive)
  (pcase (hym/git-delta-diff--location-at-point)
    ('nil (hym/git-delta-diff--goto-stat-file))
    (`(,path removed ,_) (user-error "%s was deleted in this diff" path))
    (`(,path ,_ ,line)
     (find-file (expand-file-name path hym/git-delta-diff--directory))
     (goto-char (point-min))
     (forward-line (1- line)))))

(defun hym/git-delta-diff--goto-heading (level backward)
  (let ((target (save-excursion
                  (if backward (beginning-of-line) (end-of-line))
                  (and (outline-search-text-property 'hym/git-delta-heading level nil nil backward)
                       (line-beginning-position)))))
    (unless target
      (user-error "No %s %s" (if backward "previous" "next") (if (= level 1) "file" "hunk")))
    (goto-char target)))

(defun hym/git-delta-diff-next-file ()
  "Move to the next file header."
  (interactive)
  (hym/git-delta-diff--goto-heading 1 nil)
  (hym/git-delta-diff--scroll-to-top))

(defun hym/git-delta-diff-previous-file ()
  "Move to the previous file header."
  (interactive)
  (hym/git-delta-diff--goto-heading 1 t)
  (hym/git-delta-diff--scroll-to-top))

(defun hym/git-delta-diff-next-hunk ()
  "Move to the next hunk header."
  (interactive)
  (hym/git-delta-diff--goto-heading 2 nil))

(defun hym/git-delta-diff-previous-hunk ()
  "Move to the previous hunk header."
  (interactive)
  (hym/git-delta-diff--goto-heading 2 t))

(defun hym/git-delta-diff-hide-all ()
  "Collapse the diff to its list of files."
  (interactive)
  (outline-hide-sublevels 1))

(defun hym/git-delta-diff--delta-command (width)
  (format "delta --side-by-side --hunk-header-decoration-style underline --width %d" width))

(defun hym/git-delta-diff--match-text (match)
  (string-trim (buffer-substring (prop-match-beginning match) (prop-match-end match))))

(defun hym/git-delta-diff--sticky-header-at (pos)
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (when-let ((nearest (text-property-search-backward 'hym/git-delta-heading)))
      (if (get-text-property (prop-match-beginning nearest) 'hym/git-delta-hunk)
          (concat (hym/git-delta-diff--match-text
                   (text-property-search-backward 'hym/git-delta-file))
                  " — "
                  (hym/git-delta-diff--match-text nearest))
        (hym/git-delta-diff--match-text nearest)))))

(defun hym/git-delta-diff--sticky-header ()
  (hym/git-delta-diff--sticky-header-at (window-start)))

(define-derived-mode hym/git-delta-diff-mode special-mode "Delta"
  (setq-local outline-search-function #'hym/git-delta-diff--outline-search
              outline-level #'hym/git-delta-diff--outline-level
              imenu-create-index-function #'hym/git-delta-diff--imenu-index
              header-line-format '(:eval (hym/git-delta-diff--sticky-header)))
  (outline-minor-mode 1))

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
        ;; Delta colours nearly every token, and the default face function
        ;; makes one overlay per run — hundreds of thousands on a large diff,
        ;; which cripples redisplay and GC. Text properties cost far less.
        (ansi-color-apply-face-function #'ansi-color-apply-text-property-face)
        ;; Nothing here is editable, and each refresh rewrites the whole
        ;; buffer, so undo would just accumulate copies of the old diff.
        (buffer-undo-list t)
        (default-directory hym/git-delta-diff--directory)
        (pos (point))
        (folds (hym/git-delta-diff--collapsed-headings)))
    (erase-buffer)
    (call-process-shell-command (funcall hym/git-delta-diff--command-fn) nil t)
    (if (= (point-min) (point-max))
        (insert "No changes")
      (ansi-color-apply-on-region (point-min) (point-max))
      (hym/git-delta-diff--mark-headings)
      (hym/git-delta-diff--restore-folds folds))
    (setq hym/git-delta-diff--rendered-width (hym/git-delta-diff--width))
    (goto-char (min pos (point-max)))))

(defun hym/git-delta-diff--show-buffer (buf)
  "Display BUF in a tab in the current tab group.
Reuse an existing tab only when it belongs to the current group."
  (let* ((group (and (fboundp 'hym/tab-group) (hym/tab-group)))
         (tabs (tab-bar-get-buffer-tab buf nil nil t))
         (tab (seq-find
               (lambda (candidate)
                 (or (null group)
                     (equal (hym/tab-group candidate) group)))
               tabs))
         (index (and tab
                     (seq-position
                      (funcall tab-bar-tabs-function) tab #'equal)))
         (position (and index (1+ index))))
    (if position
      (progn
        (tab-bar-select-tab position)
        (switch-to-buffer buf))
      (switch-to-buffer-other-tab buf)
      (tab-bar-rename-tab "delta"))))

(defun hym/git-delta-diff--workspace-name ()
  "Return the current workspace name, or the current tab group as a fallback."
  (if-let ((ws (and (fboundp 'hym-workspace-current)
                    (hym-workspace-current))))
      (hym-workspace-name ws)
    (if (fboundp 'hym/tab-group)
        (hym/tab-group)
      "global")))

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
         (buf (get-buffer-create
               (format "*delta-diff[%s]: %s/%s*"
                       diff-type
                       (hym/git-delta-diff--workspace-name)
                       (project-name (project-current))))))
    (with-current-buffer buf
      (hym/git-delta-diff-mode)
      (setq-local hym/git-delta-diff--directory dir)
      (setq-local hym/git-delta-diff--command-fn
                  (or command-fn
                      (lambda ()
                        (format "{ GIT_PAGER=cat git diff --stat %1$s; echo; GIT_PAGER=cat git diff -U5 %1$s | %2$s; }"
                                (or args "")
                                (hym/git-delta-diff--delta-command (hym/git-delta-diff--width))))))
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (insert "Loading diff…")
        (goto-char (point-min)))
      (evil-local-set-key 'normal "q" 'tab-close)
      (evil-local-set-key 'normal "gr" 'hym/git-delta-diff-refresh)
      (evil-local-set-key 'normal (kbd "RET") 'hym/git-delta-diff-visit-file)
      (evil-local-set-key 'normal "]f" 'hym/git-delta-diff-next-file)
      (evil-local-set-key 'normal "[f" 'hym/git-delta-diff-previous-file)
      (evil-local-set-key 'normal "]c" 'hym/git-delta-diff-next-hunk)
      (evil-local-set-key 'normal "[c" 'hym/git-delta-diff-previous-hunk)
      (evil-local-set-key 'normal "zM" 'hym/git-delta-diff-hide-all))
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
     (format "{ GIT_PAGER=cat git diff --stat; git ls-files --others --exclude-standard | while IFS= read -r f; do GIT_PAGER=cat git diff --stat --no-index /dev/null \"$f\"; done; echo; { GIT_PAGER=cat git diff -U5; git ls-files --others --exclude-standard | while IFS= read -r f; do GIT_PAGER=cat git diff --no-index /dev/null \"$f\"; done; } | %s; }"
             (hym/git-delta-diff--delta-command (hym/git-delta-diff--width))))))

(defun hym/git-delta-diff-merge-base (&optional base-branch)
  "Show delta diff from merge base with BASE-BRANCH or the default branch.
Prefers the remote-tracking ref over the local branch: worktree branches
fork from origin/<base>, so a stale local default branch would drag
unrelated upstream commits into the diff."
  (interactive)
  (let* ((default-directory (magit-toplevel))
         (branch (or base-branch (magit-main-branch)))
         (remote-branch (concat "origin/" branch))
         (base (if (magit-rev-verify remote-branch) remote-branch branch))
         (merge-base (magit-git-string "merge-base" base "HEAD")))
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
         (lambda () (format "git diff --no-index /dev/null %s | %s"
                            qf (hym/git-delta-diff--delta-command (hym/git-delta-diff--width)))))))
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

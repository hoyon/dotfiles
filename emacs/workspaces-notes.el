;; -*- lexical-binding: t -*-

(defun hym-workspace--notes-file (ws)
  "Path to WS's persistent org notes file."
  (expand-file-name "notes.org" (hym-workspace-dir ws)))

(defun hym-workspace--scratch-file (ws)
  "Path to WS's persistent (plain-text) scratch file."
  (expand-file-name "scratch" (hym-workspace-dir ws)))

(defun hym-workspace--open-file-tab (ws name file)
  "Show FILE in a tab NAME in WS's group, reusing a tab already showing it.
When FILE's buffer is already displayed in some tab, switch to that tab
instead of spawning a duplicate."
  (make-directory (file-name-directory file) t)
  (let* ((buf (get-file-buffer file))
         (tab (and buf (tab-bar-get-buffer-tab buf))))
    (if tab
        (progn
          (tab-bar-switch-to-tab (alist-get 'name tab))
          (switch-to-buffer buf))
      (hym-workspace-spawn-tab ws name (lambda () (find-file file))))))

(defun hym-workspace-notes ()
  "Open the current workspace's persistent org notes, reusing its tab."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (hym-workspace--open-file-tab ws "notes" (hym-workspace--notes-file ws))))

(defun hym-workspace-scratch ()
  "Open the current workspace's persistent scratch buffer, reusing its tab.
The file has no extension, so it opens in `fundamental-mode'."
  (interactive)
  (when-let ((ws (hym-workspace-current)))
    (hym-workspace--open-file-tab ws "scratch" (hym-workspace--scratch-file ws))))

(provide 'hym-workspaces-notes)

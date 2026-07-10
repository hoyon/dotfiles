;; -*- lexical-binding: t -*-

(defface hym-workspace-sidebar-current
  '((t :inherit bold))
  "Face for the name of the workspace you are currently in."
  :group 'hym-workspace)

(defface hym-workspace-sidebar-current-bg
  '((t :inherit hl-line :extend t))
  "Background face for the current workspace card.
Ef themes override this with their `bg-active' palette color."
  :group 'hym-workspace)

(with-eval-after-load 'ef-themes
  (defun hym-workspace-sidebar-ef-theme-faces ()
    "Use Ef theme colors for workspace sidebar faces."
    (ef-themes-with-colors
      (set-face-attribute 'hym-workspace-sidebar-current-bg nil
                          :background bg-dim
                          :extend t)))
  (add-hook 'ef-themes-post-load-hook #'hym-workspace-sidebar-ef-theme-faces)
  (hym-workspace-sidebar-ef-theme-faces))

(defface hym-workspace-sidebar-name
  '((t :inherit default))
  "Face for a workspace name."
  :group 'hym-workspace)

(defface hym-workspace-sidebar-repo
  '((t :inherit shadow))
  "Face for a repo listed under a worktree workspace."
  :group 'hym-workspace)

(defvar hym-workspace-sidebar-status-functions nil
  "Functions contributing status lines to a workspace card.
Each is called with a workspace plist and returns a list of strings
\(propertize them as you like) shown indented beneath the workspace, or
nil.  This is how later features surface state such as a running server
or an agent waiting for input without the sidebar knowing about them.")

(defcustom hym-workspace-sidebar-width 30
  "Width of the workspace sidebar side window."
  :type 'integer :group 'hym-workspace)

(defun hym-workspace-sidebar--line-target (line)
  "Append an invisible stretch target to LINE for full-row mouse handling."
  (concat line
          (propertize " " 'display '(space :align-to right-fringe))))

(defun hym-workspace-sidebar--face (face current)
  "Return FACE merged with the current-workspace background when CURRENT."
  (if current
      (list face 'hym-workspace-sidebar-current-bg)
    face))

(defun hym-workspace-sidebar--badges (ws)
  "Collect status lines for WS from `hym-workspace-sidebar-status-functions'."
  (apply #'append
         (mapcar (lambda (f) (funcall f ws))
                 hym-workspace-sidebar-status-functions)))

(defun hym-workspace-sidebar--repos (ws current)
  "Return repo detail lines for worktree WS."
  (when (eq (hym-workspace-type ws) 'worktree)
    (let ((face (hym-workspace-sidebar--face
                 'hym-workspace-sidebar-repo current)))
      (mapcar (lambda (repo) (propertize repo 'face face))
              (hym-workspace-repos ws)))))

(defun hym-workspace-sidebar--card (ws &optional index)
  "Return a propertized multi-line card block for WS.
When INDEX is non-nil, show it as the workspace jump number."
  (let* ((name (hym-workspace-name ws))
         (current (equal name (hym/tab-group)))
         (dot (when (eq (hym-workspace-type ws) 'worktree)
                (if (hym-workspace-open-p ws) "●" "○")))
         (prefix (when index (number-to-string index)))
         (leader (cond ((and prefix dot) (format "%s %s " prefix dot))
                       (prefix (format "%s " prefix))
                       (dot (format "%s " dot))
                       (t "")))
         (indent (make-string (string-width leader) ?\s))
         (name-face (if current
                        (hym-workspace-sidebar--face
                         'hym-workspace-sidebar-current current)
                      'hym-workspace-sidebar-name))
         (lines (append
                 (list (concat leader
                               (propertize name 'face name-face)))
                 (mapcar (lambda (repo) (concat indent repo))
                         (hym-workspace-sidebar--repos ws current))
                 (mapcar (lambda (b) (concat indent b))
                         (hym-workspace-sidebar--badges ws))))
         (block (concat (mapconcat #'identity
                                    (mapcar #'hym-workspace-sidebar--line-target
                                            lines)
                                    "\n")
                        "\n")))
    (when current
      (add-face-text-property 0 (length block)
                              'hym-workspace-sidebar-current-bg nil block))
    (add-text-properties 0 (length block)
                         (list 'hym-workspace name
                               'mouse-face 'highlight
                               'pointer 'hand
                               'help-echo "mouse-1: switch to this workspace")
                         block)
    block))

(defun hym-workspace-sidebar--general-card ()
  "Return the non-registry group-zero card for the catch-all space."
  (let* ((name hym/default-tab-group)
         (current (equal name (hym/tab-group)))
         (name-face (if current
                        (hym-workspace-sidebar--face
                         'hym-workspace-sidebar-current current)
                      'hym-workspace-sidebar-name))
         (block (concat
                 (hym-workspace-sidebar--line-target
                  (concat "0 " (propertize name 'face name-face)))
                 "\n")))
    (when current
      (add-face-text-property 0 (length block)
                              'hym-workspace-sidebar-current-bg nil block))
    (add-text-properties 0 (length block)
                         (list 'hym-workspace name
                               'mouse-face 'highlight
                               'pointer 'hand
                               'help-echo "mouse-1: switch to general")
                         block)
    block))

(defvar hym-workspace-sidebar--point-name nil
  "Workspace the sidebar cursor should rest on across re-renders.
Tracked explicitly because switching a workspace restores a per-tab
window configuration that clobbers the shared sidebar's point, so the
buffer's own point is not a reliable record of where the user was.")

(defvar hym-workspace-sidebar--point-line nil
  "Buffer line to restore point to, used when it still shows the same
workspace as `hym-workspace-sidebar--point-name'.  Captured at the time
of the user's action, before a workspace switch clobbers the point, so
the cursor stays exactly where it was rather than snapping to the card.")

(defun hym-workspace-sidebar--at-point ()
  "Return the workspace name on the current line, or nil."
  (get-text-property (line-beginning-position) 'hym-workspace))

(defun hym-workspace-sidebar--goto-workspace (name)
  "Move point to NAME's card, or to the top when absent."
  (goto-char (point-min))
  (when name
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (if (equal (get-text-property (point) 'hym-workspace) name)
            (setq found t)
          (forward-line 1)))
      (unless found (goto-char (point-min))))))

(defvar hym-workspace-sidebar--show-archived nil
  "Whether the archived workspaces section is expanded.")

(defun hym-workspace-sidebar--render ()
  "Fill the current buffer with the active workspace list.
Restore point to the exact remembered line when it still shows the
remembered workspace, else to that workspace's card, keeping the window's
point in sync."
  (let ((inhibit-read-only t)
        (name (or hym-workspace-sidebar--point-name
                  (hym-workspace-sidebar--at-point)))
        (line hym-workspace-sidebar--point-line))
    (erase-buffer)
    (insert (propertize " WORKSPACES\n\n" 'face 'bold))
    (insert (hym-workspace-sidebar--general-card))
    (insert "\n")
    (let ((i 0))
      (dolist (ws (hym-workspace-active))
        (setq i (1+ i))
        (insert (hym-workspace-sidebar--card ws i))
        (insert "\n")))
    (let ((archived (and hym-workspace-sidebar--show-archived
                         (hym-workspace-archived))))
      (when archived
        (insert (propertize "\n ARCHIVED\n\n" 'face 'shadow))
        (dolist (ws archived)
          (insert (hym-workspace-sidebar--card ws))
          (insert "\n"))))
    (unless (and line
                 (progn
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (equal (get-text-property (line-beginning-position)
                                             'hym-workspace)
                          name)))
      (hym-workspace-sidebar--goto-workspace name))
    (when-let ((win (get-buffer-window (current-buffer) t)))
      (set-window-point win (point)))))

(defun hym-workspace-sidebar--remember-point ()
  "Record the workspace and line at point so re-renders can restore them."
  (when-let ((name (hym-workspace-sidebar--at-point)))
    (setq hym-workspace-sidebar--point-name name
          hym-workspace-sidebar--point-line (line-number-at-pos))))

(defvar hym-workspace-sidebar-buffer-name "*workspaces*")

(define-derived-mode hym-workspace-sidebar-mode special-mode "Workspaces"
  "Major mode for the workspace sidebar."
  (setq-local cursor-type nil)
  (setq buffer-read-only t)
  (add-hook 'post-command-hook #'hym-workspace-sidebar--remember-point nil t))

(defun hym-workspace-sidebar-refresh ()
  "Re-render the sidebar buffer if it exists."
  (interactive)
  (if (derived-mode-p 'hym-workspace-sidebar-mode)
      (hym-workspace-sidebar--render)
    (when-let ((buf (get-buffer hym-workspace-sidebar-buffer-name)))
      (with-current-buffer buf (hym-workspace-sidebar--render)))))

(defun hym-workspace-sidebar--get-buffer ()
  (let ((buf (get-buffer-create hym-workspace-sidebar-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'hym-workspace-sidebar-mode)
        (hym-workspace-sidebar-mode))
      (hym-workspace-sidebar--render))
    buf))

(defvar hym-workspace-sidebar--visible nil
  "Whether the sidebar should be shown, so it survives tab-config restores.")

(defun hym-workspace-sidebar--show ()
  (display-buffer-in-side-window
   (hym-workspace-sidebar--get-buffer)
   `((side . left) (window-width . ,hym-workspace-sidebar-width)
     (dedicated . t)
     (preserve-size . (t . nil))
     (window-parameters . ((no-delete-other-windows . t))))))

(defun hym-workspace-sidebar-toggle ()
  "Toggle the workspace sidebar in a left side window."
  (interactive)
  (let ((win (get-buffer-window hym-workspace-sidebar-buffer-name)))
    (if win
        (progn
          (setq hym-workspace-sidebar--visible nil)
          (delete-window win))
      (setq hym-workspace-sidebar--visible t)
      (hym-workspace-sidebar--show))))

(defun hym-workspace-sidebar--fix-width (&rest _)
  "Re-enforce the configured sidebar width after window changes.
Normal Emacs operations (splits, `enlarge-window', `balance-windows', etc.)
can resize the side window away from `hym-workspace-sidebar-width'; this
puts it back."
  (when-let ((win (get-buffer-window hym-workspace-sidebar-buffer-name)))
    (unless (= (window-width win) hym-workspace-sidebar-width)
      (condition-case nil
          (window-resize win (- hym-workspace-sidebar-width (window-width win))
                         t)
        (error nil)))))

(defun hym-workspace-sidebar--ensure-window (&rest _)
  "Re-display the sidebar in its side window when it should be visible.
A tab is a saved window configuration, so opening or switching tabs
restores a layout without the side window; this puts it back."
  (when (and hym-workspace-sidebar--visible
             (not (get-buffer-window hym-workspace-sidebar-buffer-name)))
    (hym-workspace-sidebar--show))
  (hym-workspace-sidebar--fix-width))

(defun hym-workspace-sidebar--sync (&rest _)
  "Keep the sidebar present and its open/closed marks current."
  (hym-workspace-sidebar--ensure-window)
  (hym-workspace-sidebar-refresh))

(add-hook 'tab-bar-tab-post-open-functions #'hym-workspace-sidebar--sync)
(add-hook 'tab-bar-tab-post-select-functions #'hym-workspace-sidebar--sync)
(advice-add 'tab-bar-change-tab-group :after #'hym-workspace-sidebar--sync)
(add-hook 'hym-workspace-after-open-hook #'hym-workspace-sidebar--sync)
(add-hook 'hym-workspace-registry-change-hook #'hym-workspace-sidebar-refresh)
(add-hook 'window-configuration-change-hook #'hym-workspace-sidebar--fix-width)

(defun hym-workspace-sidebar-visit ()
  "Open or switch to the workspace on the current line."
  (interactive)
  (when-let ((name (hym-workspace-sidebar--at-point)))
    (setq hym-workspace-sidebar--point-name name
          hym-workspace-sidebar--point-line (line-number-at-pos))
    (if (equal name hym/default-tab-group)
        (hym/tab-switch-to-default-group)
      (when-let ((ws (hym-workspace-get name)))
        (hym-workspace-open ws)))
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace-sidebar-close-ws ()
  "Close (tear down tabs of) the workspace on the current line."
  (interactive)
  (when-let ((ws (hym-workspace-get (hym-workspace-sidebar--at-point))))
    (hym-workspace-close ws)
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace-sidebar-retry ()
  "Retry provisioning for the worktree workspace on the current line."
  (interactive)
  (when-let* ((name (hym-workspace-sidebar--at-point))
              (ws (hym-workspace-get name)))
    (when (and (eq (hym-workspace-type ws) 'worktree)
               (fboundp 'hym-workspace-provision-retry))
      (hym-workspace-provision-retry ws))))

(defun hym-workspace-sidebar-archive ()
  "Archive the workspace on the current line."
  (interactive)
  (when-let* ((name (hym-workspace-sidebar--at-point))
              (ws (hym-workspace-get name)))
    (if (and (eq (hym-workspace-type ws) 'worktree)
             (fboundp 'hym-workspace-archive-worktree))
        (when (yes-or-no-p
               (format "Archive %s? Removes its worktrees; uncommitted changes are lost. "
                       name))
          (hym-workspace-archive-worktree ws))
      (hym-workspace-close ws)
      (hym-workspace-put (plist-put (copy-sequence ws) :archived t)))
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace-sidebar-new ()
  "Create a new workspace, then refresh the sidebar."
  (interactive)
  (call-interactively #'hym-workspace-new)
  (hym-workspace-sidebar-refresh))

(defun hym-workspace-sidebar-toggle-archived ()
  "Show or hide the archived workspaces section."
  (interactive)
  (setq hym-workspace-sidebar--show-archived
        (not hym-workspace-sidebar--show-archived))
  (hym-workspace-sidebar-refresh))

(defun hym-workspace-sidebar-unarchive ()
  "Unarchive the workspace on the current line."
  (interactive)
  (when-let* ((name (hym-workspace-sidebar--at-point))
              (ws (hym-workspace-get name)))
    (when (hym-workspace-archived-p ws)
      (if (and (eq (hym-workspace-type ws) 'worktree)
               (fboundp 'hym-workspace-unarchive))
          (hym-workspace-unarchive ws)
        (hym-workspace-put (plist-put (copy-sequence ws) :archived nil)))
      (hym-workspace-sidebar-refresh))))

(defun hym-workspace-sidebar-add-repo ()
  "Add a repo to the worktree workspace on the current line."
  (interactive)
  (when-let* ((name (hym-workspace-sidebar--at-point))
              (ws (hym-workspace-get name)))
    (when (and (eq (hym-workspace-type ws) 'worktree)
               (fboundp 'hym-workspace-add-repo))
      (hym-workspace-add-repo
       ws (completing-read "Add repo: "
                           (seq-difference (hym-workspace--available-repos)
                                           (hym-workspace-repos ws))
                           nil t)))))

(defun hym-workspace-sidebar-rename (new-name)
  "Rename the workspace on the current line to NEW-NAME."
  (interactive (list (read-string "New name: " (hym-workspace-sidebar--at-point))))
  (when-let* ((old (hym-workspace-sidebar--at-point))
              (ws (hym-workspace-get old)))
    (setq hym-workspace-sidebar--point-name new-name)
    (hym-workspace-rename ws new-name)
    (hym-workspace-sidebar-refresh)))

(defun hym-workspace-sidebar-mouse-visit (event)
  "Switch to the workspace clicked with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (hym-workspace-sidebar-visit))

(let ((map hym-workspace-sidebar-mode-map))
  (define-key map (kbd "RET") #'hym-workspace-sidebar-visit)
  (define-key map [mouse-1] #'hym-workspace-sidebar-mouse-visit)
  (define-key map "d" #'hym-workspace-sidebar-close-ws)
  (define-key map "x" #'hym-workspace-sidebar-archive)
  (define-key map "c" #'hym-workspace-sidebar-new)
  (define-key map "+" #'hym-workspace-sidebar-new)
  (define-key map "g" #'hym-workspace-sidebar-refresh)
  (define-key map (kbd "TAB") #'hym-workspace-sidebar-toggle-archived)
  (define-key map "a" #'hym-workspace-sidebar-add-repo)
  (define-key map "u" #'hym-workspace-sidebar-unarchive)
  (define-key map "r" #'hym-workspace-sidebar-rename)
  (define-key map "!" #'hym-workspace-sidebar-retry))

(when (fboundp 'evil-define-key)
  (evil-set-initial-state 'hym-workspace-sidebar-mode 'normal)
  (evil-define-key 'normal hym-workspace-sidebar-mode-map
    (kbd "RET") #'hym-workspace-sidebar-visit
    [mouse-1] #'hym-workspace-sidebar-mouse-visit
    "d" #'hym-workspace-sidebar-close-ws
    "x" #'hym-workspace-sidebar-archive
    "c" #'hym-workspace-sidebar-new
    "+" #'hym-workspace-sidebar-new
    "g" #'hym-workspace-sidebar-refresh
    (kbd "TAB") #'hym-workspace-sidebar-toggle-archived
    "a" #'hym-workspace-sidebar-add-repo
    "u" #'hym-workspace-sidebar-unarchive
    "r" #'hym-workspace-sidebar-rename
    "!" #'hym-workspace-sidebar-retry))

(provide 'hym-workspaces-sidebar)

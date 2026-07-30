;;; workspaces.el --- Per-workspace agentic-coding harness  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; An in-Emacs, cmux/conductor-style harness for agentic coding: a left
;; sidebar of "workspaces" you switch between, each with its own tabs
;; (terminals, an agent, diffs, notes), git-worktree provisioning, and live
;; status badges. Replaces the Electron tool (Orca/cohort) for this workflow.
;;
;; Core model
;; ----------
;; A workspace IS a tab-bar group (from tabs.el) PLUS a plist in the registry.
;; "The current workspace" is read live from the current tab's group
;; (`hym-workspace-current') — there is no separate global to fall out of sync.
;; The registry is a list of plists persisted to `hym-workspace-registry-file'
;; (default <hym-workspace-home>/registry.eld). Each entry:
;;
;;   (:name "auth refactor"   ; display name = the tab-group name; renamable
;;    :slug "auth_refactor"   ; frozen id for worktree types: dir/branch/DB name
;;    :type worktree          ; worktree | directory
;;    :root "~/workspaces/auth_refactor"
;;    :repos ("ploy-server" "ploy-client")  ; (".") for directory
;;    :base-branch "main" :archived nil)
;;
;; `hym-workspace-home' (~/workspaces) is the one home: the registry plus a dir
;; per workspace (`hym-workspace-dir' = <home>/<key>) holding its worktrees,
;; notes, scratch, and any ad-hoc scripts. It survives archive (which only
;; removes the sub-repo worktrees).
;;
;; Module map
;; ----------
;;   workspaces.el          this file: registry + persistence, accessors,
;;                          lifecycle (open/close/switch/current),
;;                          `hym-workspace-mode' (top bar shows only the current
;;                          group's tabs), `hym-workspace-new' type dispatch,
;;                          rename, slugify/key/dir, and the extension seams.
;;   workspaces-sidebar.el  the left side-window picker: cards, badges, keys,
;;                          archived section.
;;   workspaces-worktree.el git-worktree provisioning from conductor.json
;;                          (async), add-repo, archive/unarchive, retry.
;;   workspaces-run.el      ghostel shell/server/agent tabs; server + agent
;;                          badges; `hym-workspace-agent-signal'.
;;   workspaces-git.el      per-repo magit status / delta PR-diff / log.
;;   workspaces-notes.el    per-workspace org notes + plain scratch.
;;   claude/hooks/          hym-agent-hook + install-hooks: Claude/Codex/goose
;;                          lifecycle events -> emacsclient ->
;;                          `hym-workspace-agent-signal'.
;;                          Run install-hooks once per machine.
;;
;; Extension seams (how to add things without touching internals)
;; --------------------------------------------------------------
;;   `hym-workspace-spawn-tab'                 open a new tab in a workspace
;;   `hym-workspace-sidebar-status-functions'  add a sidebar badge
;;   `hym-workspace-type-creators'             provision a new workspace type
;;   `hym-workspace-type-handlers'             per-type archive/unarchive/
;;                                             add-repo/retry, so the sidebar
;;                                             never needs to know about
;;                                             worktrees
;;   `hym-workspace-agents'                    add a coding agent
;;   `hym-workspace-ui-refresh-hook'           redraw whatever shows workspace
;;                                             state (the sidebar adds itself)
;;   `hym-workspace-teardown-functions'        stop what you started in a
;;                                             workspace before it is archived
;;   `hym-workspace-open-hook' / -after-open-hook / -registry-change-hook
;;
;; Tests live beside each module as <module>-test.el; `make test' in this
;; directory runs them all.
;;
;; Keys (leader `SPC o'): oo sidebar, on new, oa add-repo, ot shell, or server,
;; oc Ghostty agent, oC agent-shell, og/od/ol git status/diff/log, oN notes,
;; os scratch; `SPC t j/k/t' and `SPC 1-9' switch between workspaces; `SPC 0'
;; switches to the non-registry general group.

;;; Code:
(require 'seq)

(defgroup hym-workspace nil
  "In-Emacs workspace harness."
  :group 'convenience)

(defcustom hym-workspace-home "~/workspaces"
  "Top-level home for the harness: the registry file and one directory per
workspace (worktrees, notes, scratch, ad-hoc scripts)."
  :type 'directory :group 'hym-workspace)

(defcustom hym-workspace-registry-file
  (expand-file-name "registry.eld" hym-workspace-home)
  "File persisting the workspace registry."
  :type 'file :group 'hym-workspace)

(defvar hym-workspace--registry nil
  "In-memory registry: a list of workspace plists.")
(defvar hym-workspace--loaded nil
  "Non-nil once the registry has been read from disk this session.")
(defvar hym-workspace--load-failed nil
  "Non-nil when the registry file existed but could not be parsed.")

(defvar hym-workspace-registry-change-hook nil
  "Run after the registry is mutated by `hym-workspace-put' or -remove'.")

(defvar hym-workspace-after-open-hook nil
  "Run after any workspace is opened or switched to via `hym-workspace-open'.")

(defvar hym-workspace-ui-refresh-hook nil
  "Run when workspace state changes in a way the UI should reflect.
The sidebar adds itself here; anything tracking live state (provisioning,
servers, agents) calls `hym-workspace-refresh-ui' rather than reaching
for the sidebar directly.")

(defun hym-workspace-refresh-ui ()
  "Redraw anything displaying workspace state."
  (run-hooks 'hym-workspace-ui-refresh-hook))

(defvar hym-workspace-teardown-functions nil
  "Functions called with a workspace before its resources are removed.
Each feature owning something long-lived in a workspace (servers, agent
terminals) registers a cleanup here rather than being called by name
from the archiving code.")

(defun hym-workspace-run-teardown (ws)
  "Let every feature clean up after WS before its files go."
  (run-hook-with-args 'hym-workspace-teardown-functions ws))

(defun hym-workspace--read-eld (file what)
  "Read the single sexp in FILE, or nil when it does not exist.
WHAT names the file's contents in the error signalled for unparseable
contents, so a corrupt file is never silently treated as empty."
  (when (file-exists-p file)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (read (current-buffer)))
      (error
       (error "Corrupt %s %s: %s" what file (error-message-string err))))))

(defun hym-workspace-load ()
  "Load the registry from `hym-workspace-registry-file'.
Signal rather than silently returning an empty registry when the file
is present but unreadable, so a later save cannot clobber it."
  (setq hym-workspace--load-failed nil)
  (setq hym-workspace--registry
        (condition-case err
            (hym-workspace--read-eld hym-workspace-registry-file
                                     "workspace registry")
          (error
           (setq hym-workspace--load-failed t)
           (error "%s (fix or delete it)" (error-message-string err)))))
  (setq hym-workspace--loaded t)
  hym-workspace--registry)

(defun hym-workspace-registry ()
  "Return the workspace registry, loading it on first use."
  (unless hym-workspace--loaded (hym-workspace-load))
  hym-workspace--registry)

(defun hym-workspace-save ()
  "Persist the registry to `hym-workspace-registry-file'."
  (when hym-workspace--load-failed
    (error "Refusing to save: workspace registry failed to load; resolve %s first"
           hym-workspace-registry-file))
  (make-directory (file-name-directory hym-workspace-registry-file) t)
  (with-temp-file hym-workspace-registry-file
    (let ((print-length nil) (print-level nil))
      (pp hym-workspace--registry (current-buffer)))))

(defun hym-workspace-get (name)
  "Return the workspace plist named NAME, or nil."
  (seq-find (lambda (w) (equal (plist-get w :name) name))
            (hym-workspace-registry)))

(defun hym-workspace-put (ws)
  "Insert or replace WS in the registry by `:name', preserving position."
  (let* ((name (plist-get ws :name))
         (reg (hym-workspace-registry))
         (pos (seq-position reg name
                            (lambda (w n) (equal (plist-get w :name) n)))))
    (setq hym-workspace--registry
          (if pos
              (append (seq-take reg pos) (list ws) (seq-drop reg (1+ pos)))
            (append reg (list ws))))
    (hym-workspace-save)
    (run-hooks 'hym-workspace-registry-change-hook)
    ws))

(defun hym-workspace-remove (name)
  "Remove the workspace named NAME from the registry."
  (setq hym-workspace--registry
        (seq-remove (lambda (w) (equal (plist-get w :name) name))
                    (hym-workspace-registry)))
  (hym-workspace-save)
  (run-hooks 'hym-workspace-registry-change-hook))

(defun hym-workspace-update (ws &rest props)
  "Apply PROPS (a plist) to WS's registry entry and persist it.
The entry is re-read by name first, so a caller holding a copy taken
before an async operation cannot roll back whatever landed meanwhile.
WS itself is left untouched. Returns the stored workspace, or nil when
WS is no longer registered."
  (when-let ((current (hym-workspace-get (hym-workspace-name ws))))
    (let ((updated (copy-sequence current)))
      (while props
        (setq updated (plist-put updated (pop props) (pop props))))
      (hym-workspace-put updated))))

;;;; Accessors

(defun hym-workspace-name (ws) (plist-get ws :name))
(defun hym-workspace-type (ws) (plist-get ws :type))
(defun hym-workspace-root (ws) (expand-file-name (plist-get ws :root)))
(defun hym-workspace-repos (ws) (or (plist-get ws :repos) '(".")))
(defun hym-workspace-base-branch (ws) (plist-get ws :base-branch))
(defun hym-workspace-archived-p (ws) (plist-get ws :archived))
(defun hym-workspace-slug (ws) (plist-get ws :slug))

(defun hym-workspace-active ()
  "Return non-archived workspaces in registry order."
  (seq-remove #'hym-workspace-archived-p (hym-workspace-registry)))

(defun hym-workspace-archived ()
  "Return archived workspaces in registry order."
  (seq-filter #'hym-workspace-archived-p (hym-workspace-registry)))

(defun hym-workspace-repo-dirs (ws)
  "Return absolute, slash-terminated directories to run git/server ops in for WS."
  (let ((root (hym-workspace-root ws)))
    (mapcar (lambda (r) (file-name-as-directory (expand-file-name r root)))
            (hym-workspace-repos ws))))

(defvar hym-workspace-open-hook nil
  "Hook run in a workspace's context right after it is opened.
The workspace is available via `hym-workspace-current'.")

(defvar hym-workspace-first-tab-name "files"
  "Name of the dired tab seeded when a workspace is first opened.")

(defun hym-workspace-current ()
  "Return the workspace plist for the current tab's group, or nil."
  (hym-workspace-get (hym/tab-group)))

(defun hym-workspace-open-p (ws)
  "Return non-nil when WS has a live tab group."
  (and (member (hym-workspace-name ws) (hym/tab-groups)) t))

(defun hym-workspace--seed (ws)
  "Create WS's tab group with an initial dired tab at its root."
  (let ((name (hym-workspace-name ws)))
    (hym/tab-new-in-group name)
    (dired (hym-workspace-root ws))
    (tab-bar-rename-tab hym-workspace-first-tab-name)))

(defun hym-workspace-open (ws)
  "Open WS: switch to its group if live, else create and seed it."
  (if (hym-workspace-open-p ws)
      (hym/tab-group-switch-to (hym-workspace-name ws))
    (hym-workspace--seed ws)
    (run-hooks 'hym-workspace-open-hook))
  (run-hooks 'hym-workspace-after-open-hook)
  ws)

(defun hym-workspace-close (ws)
  "Tear down WS's tab group; leave its registry entry intact."
  (when (hym-workspace-open-p ws)
    (tab-bar-close-group-tabs (hym-workspace-name ws))))

(defun hym-workspace-spawn-tab (ws name setup)
  "Create a tab named NAME in WS's group and call SETUP (a function) in it.
This is the seam every later feature (notes, scratch, git, server) uses."
  (hym-workspace-open ws)
  (hym/tab-new-in-group (hym-workspace-name ws))
  (tab-bar-rename-tab name)
  (funcall setup))

(defvar hym-workspace--saved-tab-bar-format nil
  "Previous `tab-bar-format', restored when `hym-workspace-mode' is disabled.")

(defun hym-workspace-format-current-group-tabs ()
  "Tab-bar format function emitting items for only the current group's tabs."
  (let ((group (hym/tab-group))
        (i 0)
        (items nil))
    (dolist (tab (funcall tab-bar-tabs-function))
      (setq i (1+ i))
      (when (equal (hym/tab-group tab) group)
        (setq items (append items (tab-bar--format-tab tab i)))))
    items))

(define-minor-mode hym-workspace-mode
  "Two-layer workspace shell: sidebar picker plus per-workspace tab bar."
  :global t
  (if hym-workspace-mode
      (unless (eq (car-safe tab-bar-format) 'hym-workspace-format-current-group-tabs)
        (setq hym-workspace--saved-tab-bar-format tab-bar-format)
        (setq tab-bar-format
              '(hym-workspace-format-current-group-tabs tab-bar-separator)))
    (setq tab-bar-format hym-workspace--saved-tab-bar-format)))

(defun hym-workspace--read-directory ()
  "Read a workspace directory, starting the picker in the home directory."
  (abbreviate-file-name
   (read-directory-name "Directory: " (expand-file-name "~/") nil t)))

(defun hym-workspace--directory-name (directory)
  "Return the final path component of DIRECTORY."
  (file-name-nondirectory
   (directory-file-name (expand-file-name directory))))

(defun hym-workspace-create (name type root &optional base-branch)
  "Create and persist a bare workspace NAME of TYPE at ROOT.
No provisioning happens here; that is Layer 1. Returns the workspace."
  (interactive
   (let ((type (hym-workspace--read-type)))
     (if (eq type 'directory)
         (let ((root (hym-workspace--read-directory)))
           (list (hym-workspace--directory-name root) type root))
       (list (read-string "Workspace name: ")
             type
             (hym-workspace--read-directory)
             (read-string "Base branch: " "main")))))
  (hym-workspace-put
   (list :name name :type type :root root
         :repos (if (eq type 'worktree) '() '("."))
         :base-branch base-branch :archived nil)))

(defun hym-workspace-switch (&optional name)
  "Switch to workspace NAME, opening it if needed.
Interactively prompt over the active (non-archived) workspaces."
  (interactive)
  (let* ((names (mapcar #'hym-workspace-name (hym-workspace-active)))
         (name (or name (completing-read "Workspace: " names nil t))))
    (when-let ((ws (hym-workspace-get name)))
      (hym-workspace-open ws))))

(defun hym-workspace--cycle (direction)
  "Switch to the next (DIRECTION 1) or previous (-1) active workspace."
  (let ((names (mapcar #'hym-workspace-name (hym-workspace-active))))
    (when names
      (let* ((pos (seq-position names (hym/tab-group) #'string=))
             (base (or pos (if (> direction 0) -1 0)))
             (next (nth (mod (+ base direction) (length names)) names)))
        (hym-workspace-switch next)))))

(defun hym-workspace-next ()
  "Switch to the next active workspace."
  (interactive)
  (hym-workspace--cycle 1))

(defun hym-workspace-prev ()
  "Switch to the previous active workspace."
  (interactive)
  (hym-workspace--cycle -1))

(defun hym-workspace-select-index (n)
  "Switch to the Nth active workspace (1-based), opening it if needed."
  (interactive "p")
  (when-let ((ws (nth (1- n) (hym-workspace-active))))
    (hym-workspace-open ws)))

(defun hym-workspace-select-index-command (n)
  "Return a command switching to the Nth active workspace."
  (lambda () (interactive) (hym-workspace-select-index n)))

(defun hym-workspace--slugify (name)
  "Return a filesystem/branch/DB-safe slug for NAME."
  (let ((s (downcase (string-trim name))))
    (setq s (replace-regexp-in-string "[^a-z0-9]+" "_" s))
    (replace-regexp-in-string "\\`_+\\|_+\\'" "" s)))

(defun hym-workspace--key (ws)
  "Return WS's stable filesystem key: its slug, or a slugified name."
  (or (hym-workspace-slug ws)
      (hym-workspace--slugify (hym-workspace-name ws))))

(defun hym-workspace-dir (ws)
  "Return WS's per-workspace directory under `hym-workspace-home'.
Holds worktrees (for worktree workspaces), notes, scratch, and ad-hoc
scripts. Not created here; callers make it as needed."
  (file-name-as-directory
   (expand-file-name (hym-workspace--key ws)
                     (expand-file-name hym-workspace-home))))

;;;; Per-type behaviour

(defvar hym-workspace-type-creators nil
  "Alist mapping a workspace TYPE symbol to an interactive creator command.
When `hym-workspace-new' is asked for a type present here, it delegates to
that command instead of creating a bare entry.")

(defvar hym-workspace-type-handlers nil
  "Alist mapping a workspace TYPE symbol to a plist of operations.
Recognised keys are `:archive', `:unarchive', `:add-repo' and `:retry',
each a function of one argument, the workspace.  Types absent here fall
back to the generic registry-only behaviour, which is how the sidebar
offers these actions without knowing which types support them.")

(defun hym-workspace-type-handler (ws op)
  "Return WS's handler function for OP, or nil when its type has none."
  (plist-get (alist-get (hym-workspace-type ws) hym-workspace-type-handlers)
             op))

(defun hym-workspace--read-type ()
  "Read a workspace type."
  (intern (completing-read "Type: " '("worktree" "directory") nil t)))

(defun hym-workspace-new ()
  "Create a workspace, prompting for type and delegating provisioning."
  (interactive)
  (let ((type (hym-workspace--read-type)))
    (if-let ((creator (alist-get type hym-workspace-type-creators)))
        (call-interactively creator)
      (let* ((root (hym-workspace--read-directory))
             (name (if (eq type 'directory)
                       (hym-workspace--directory-name root)
                     (read-string "Workspace name: "))))
        (hym-workspace-create name type root)))))

(defun hym-workspace-rename (ws new-name)
  "Rename WS's display name to NEW-NAME, keeping its registry position.
Renames the live tab group too when the workspace is open."
  (interactive (list (hym-workspace-current) (read-string "New name: ")))
  (let ((old (hym-workspace-name ws)))
    (when (and (not (equal new-name old)) (hym-workspace-get new-name))
      (user-error "A workspace named %s already exists" new-name))
    (when (hym-workspace-open-p ws)
      (hym/tab-group-rename old new-name))
    (setq hym-workspace--registry
          (mapcar (lambda (w)
                    (if (equal (plist-get w :name) old)
                        (plist-put (copy-sequence w) :name new-name)
                      w))
                  (hym-workspace-registry)))
    (hym-workspace-save)
    (run-hooks 'hym-workspace-registry-change-hook)))

(provide 'hym-workspaces)

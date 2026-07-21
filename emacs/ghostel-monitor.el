;;; ghostel-monitor.el --- Track ghostel terminals per workspace  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Ghostel terminals (and vterm buffers) can be left running in the background,
;; consuming RAM with no visible trace.
;; This module:
;;
;;   1. Adds a sidebar badge per workspace showing terminal count and total
;;      memory.
;;   2. Provides a detail buffer (`hym-ghostel-monitor') listing every terminal
;;      with PID, memory, uptime, state, child process, buffer name, and
;;      workspace — with keys to kill individual terminals.
;;
;; Cross-platform (Linux + macOS): uses `ps` for RSS, `pgrep` for child
;; detection, and `process-attributes` for state/uptime.  No /proc parsing.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defvar hym-workspace-sidebar-mode-map)
(defvar hym-workspace-sidebar-status-functions)
(declare-function hym-workspace--key "workspaces")
(declare-function hym-workspace-current "workspaces")
(declare-function hym-workspace-sidebar-refresh "workspaces-sidebar")

;; ── Customisation ───────────────────────────────────────────────────────────

(defgroup hym-ghostel-monitor nil
  "Monitor ghostel and vterm terminals across workspaces."
  :group 'hym-workspace)

(defcustom hym-ghostel-monitor-known-agents
  '("claude" "codex" "goose" "node" "python" "ruby" "java"
    "cargo" "rustc" "zig" "elixir" "iex" "mix" "npm" "yarn")
  "Process names that indicate something interesting is running.
Used to label a terminal with what it's actually doing, beyond just the
shell itself."
  :type '(repeat string)
  :group 'hym-ghostel-monitor)

(defcustom hym-ghostel-monitor-track-vterm t
  "When non-nil, also track vterm buffers alongside ghostel ones."
  :type 'boolean
  :group 'hym-ghostel-monitor)

(defcustom hym-ghostel-monitor-sidebar-refresh-interval 5
  "Minimum seconds between expensive sidebar memory refreshes.
Workspace switches should render from cached process data; the expensive
`ps'/`pgrep' scan is deferred to an idle timer."
  :type 'number
  :group 'hym-ghostel-monitor)

;; ── Buffer-local workspace tag ──────────────────────────────────────────────

(defvar-local hym-ghostel-monitor--workspace nil
  "Workspace key (from `hym-workspace--key') for this terminal buffer.
Tagged when the terminal is created inside a workspace tab.")

(defun hym-ghostel-monitor--tag-buffer ()
  "Record the current workspace on this terminal buffer."
  (when (and (fboundp 'hym-workspace-current)
             (fboundp 'hym-workspace--key))
    (when-let ((ws (hym-workspace-current)))
      (setq hym-ghostel-monitor--workspace (hym-workspace--key ws)))))

;; ── Buffer discovery ────────────────────────────────────────────────────────

(defun hym-ghostel-monitor--terminal-buffers ()
  "Return a list of all live ghostel (and optionally vterm) buffers."
  (let (bufs)
    (dolist (buf (buffer-list) (nreverse bufs))
      (ignore-errors
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (or (derived-mode-p 'ghostel-mode)
                      (and hym-ghostel-monitor-track-vterm
                           (derived-mode-p 'vterm-mode)))
              (push buf bufs))))))))

(defun hym-ghostel-monitor--terminal-buffers-for-workspace (key)
  "Return terminal buffers belonging to workspace KEY."
  (seq-filter
   (lambda (buf)
     (equal (buffer-local-value 'hym-ghostel-monitor--workspace buf) key))
   (hym-ghostel-monitor--terminal-buffers)))

(defun hym-ghostel-monitor--buffer-summary (buf)
  "Return cheap terminal summary data for BUF.
This avoids process inspection and is safe for sidebar rendering."
  (list :buffer buf
        :buffer-name (ignore-errors (buffer-name buf))
        :workspace (or (and (buffer-live-p buf)
                            (buffer-local-value 'hym-ghostel-monitor--workspace
                                                buf))
                       "-")))

;; ── Process utilities (cross-platform) ──────────────────────────────────────

(defun hym-ghostel-monitor--buffer-pid (buf)
  "Return the PID of the process in BUF, or nil."
  (or (when-let ((proc (get-buffer-process buf)))
        (ignore-errors (process-id proc)))
      ;; On macOS Ghostel uses a network process as its Emacs process
      ;; object, for which `process-id' is nil.  The native terminal's
      ;; shell PID is stored separately in this buffer-local variable.
      (when (and (buffer-live-p buf)
                 (local-variable-p 'ghostel--pid buf))
        (let ((pid (buffer-local-value 'ghostel--pid buf)))
          (and (integerp pid) (> pid 0) pid)))))

(defun hym-ghostel-monitor--process-rss-kb (pid)
  "Return the RSS in KB for PID, or 0 on failure.
Uses `ps' which works identically on Linux and macOS."
  (or (ignore-errors
        (string-to-number
         (string-trim
          (car (process-lines "ps" "-o" "rss=" "-p"
                              (number-to-string pid))))))
      0))

(defun hym-ghostel-monitor--process-children (pid)
  "Return a list of immediate child PIDs for PID.
Returns nil when `pgrep' is unavailable or PID has no children."
  (when (executable-find "pgrep")
    (ignore-errors
      (mapcar #'string-to-number
              (process-lines "pgrep" "-P" (number-to-string pid))))))

(defun hym-ghostel-monitor--process-tree-rss (pid &optional seen)
  "Sum the RSS (KB) of PID and all its descendants."
  (let ((seen (or seen (make-hash-table :test 'eql))))
    (if (gethash pid seen)
        0
      (puthash pid t seen)
      (let ((total (hym-ghostel-monitor--process-rss-kb pid)))
        (dolist (child (hym-ghostel-monitor--process-children pid) total)
          (cl-incf total
                   (hym-ghostel-monitor--process-tree-rss child seen)))))))

(defun hym-ghostel-monitor--find-interesting-child (pid &optional depth seen)
  "Walk PID's descendants up to DEPTH looking for a known agent process.
Returns the command name of the first interesting child found, or nil."
  (when (and (or (null depth) (< depth 4))
             (executable-find "pgrep")
             (not (gethash pid (or seen (make-hash-table :test 'eql)))))
    (let ((seen (or seen (make-hash-table :test 'eql))))
      (puthash pid t seen)
      (catch 'found
        (dolist (child (hym-ghostel-monitor--process-children pid))
          (unless (gethash child seen)
            (let* ((attrs (ignore-errors (process-attributes child)))
                   (comm (cdr (assq 'comm attrs))))
              (when comm
                (if (member comm hym-ghostel-monitor-known-agents)
                    (throw 'found comm)
                  (when-let ((found (hym-ghostel-monitor--find-interesting-child
                                     child (1+ (or depth 0)) seen)))
                    (throw 'found found)))))))))))

(defun hym-ghostel-monitor--buffer-info (buf)
"Return a plist of information for terminal buffer BUF.
Keys: :buffer, :buffer-name, :pid, :rss-kb, :uptime, :state, :what,
:workspace.  Never returns nil — falls back to a sane default on any error so
the reduction in the badge can always sum :rss-kb."
  (or (ignore-errors
        (when (buffer-live-p buf)
          (let* ((summary (hym-ghostel-monitor--buffer-summary buf))
                 (pid  (hym-ghostel-monitor--buffer-pid buf))
                 (attrs (and pid (process-attributes pid)))
                 (rss  (if pid (hym-ghostel-monitor--process-tree-rss pid) 0))
                 (what (if pid
                           (or (hym-ghostel-monitor--find-interesting-child pid)
                               (cdr (assq 'comm attrs))
                               "?")
                         "-"))
                 (state (or (cdr (assq 'state attrs)) "?"))
                 (etime (cdr (assq 'etime attrs)))
                 (uptime (if etime (hym-ghostel-monitor--format-uptime etime) "?")))
            (append summary
                    (list
                  :pid (or pid 0)
                  :rss-kb rss
                  :uptime uptime
                  :state state
                          :what what)))))
      ;; Fallback plist if anything above throws.
      (append (hym-ghostel-monitor--buffer-summary buf)
              (list
            :pid 0
            :rss-kb 0
            :uptime "?"
            :state "?"
                    :what "-"))))

;; ── Formatting ──────────────────────────────────────────────────────────────

(defun hym-ghostel-monitor--format-uptime (etime)
  "Convert an Emacs `etime' (seconds as a float) to a human string.
Handles both the `float' and `(HIGH LOW MICRO PSEC)' formats returned by
`process-attributes' across different platforms."
  (let* ((s (floor (if (numberp etime)
                       etime
                     (float-time etime))))
         (d (/ s 86400))
         (h (% (/ s 3600) 24))
         (m (% (/ s 60) 60)))
    (cond ((> d 0) (format "%dd %dh" d h))
          ((> h 0) (format "%dh %dm" h m))
          ((> m 0) (format "%dm" m))
          (t       (format "%ds" s)))))

(defun hym-ghostel-monitor--format-memory (rss-kb)
  "Return a human-readable memory string for RSS-KB."
  (cond ((>= rss-kb 1048576) (format "%.1f GB" (/ rss-kb 1048576.0)))
        ((>= rss-kb 1024)    (format "%.0f MB" (/ rss-kb 1024.0)))
        (t                   (format "%d KB" rss-kb))))

;; ── Sidebar badge ───────────────────────────────────────────────────────────

(defvar hym-ghostel-monitor--sidebar-cache (make-hash-table :test 'equal)
  "Cached terminal summaries keyed by workspace key.")

(defvar hym-ghostel-monitor--sidebar-cache-time 0
  "Float timestamp of the last expensive sidebar cache refresh.")

(defvar hym-ghostel-monitor--sidebar-cache-timer nil
  "Idle timer used to refresh sidebar process summaries.")

(defun hym-ghostel-monitor--summary-line (summary)
  "Return a sidebar line for SUMMARY, or nil."
  (let ((count (plist-get summary :count)))
    (when (and count (> count 0))
      (concat
       "▸ " (number-to-string count) " term" (if (> count 1) "s" "")
       " · " (hym-ghostel-monitor--format-memory
              (or (plist-get summary :rss-kb) 0))))))

(defun hym-ghostel-monitor--summaries-from-infos (infos)
  "Return a hash table of workspace summaries built from terminal INFOS."
  (let ((groups (make-hash-table :test 'equal))
        (summaries (make-hash-table :test 'equal)))
    (dolist (info infos)
      (push info (gethash (plist-get info :workspace) groups)))
    (maphash
     (lambda (key ws-infos)
       (puthash key
                (list :count (length ws-infos)
                      :rss-kb (cl-reduce #'+ ws-infos
                                         :key (lambda (info)
                                                (or (plist-get info :rss-kb) 0))
                                         :initial-value 0))
                summaries))
     groups)
    summaries))

(defun hym-ghostel-monitor--cache-sidebar-infos (infos)
  "Update sidebar summary cache from terminal INFOS."
  (setq hym-ghostel-monitor--sidebar-cache
        (hym-ghostel-monitor--summaries-from-infos infos))
  (setq hym-ghostel-monitor--sidebar-cache-time (float-time)))

(defun hym-ghostel-monitor--refresh-sidebar-cache ()
  "Refresh sidebar summary cache with a full process scan."
  (setq hym-ghostel-monitor--sidebar-cache-timer nil)
  (hym-ghostel-monitor--cache-sidebar-infos
   (mapcar #'hym-ghostel-monitor--buffer-info
           (hym-ghostel-monitor--terminal-buffers)))
  (hym-ghostel-monitor--sidebar-refresh))

(defun hym-ghostel-monitor--schedule-sidebar-cache-refresh ()
  "Schedule an idle refresh of cached sidebar process data when stale."
  (when (and (not (timerp hym-ghostel-monitor--sidebar-cache-timer))
             (> (- (float-time) hym-ghostel-monitor--sidebar-cache-time)
                hym-ghostel-monitor-sidebar-refresh-interval))
    (setq hym-ghostel-monitor--sidebar-cache-timer
          (run-with-idle-timer
           0.5 nil #'hym-ghostel-monitor--refresh-sidebar-cache))))

(defun hym-ghostel-monitor--cheap-summary (key)
  "Return a cheap summary for KEY without process memory inspection."
  (let* ((bufs (hym-ghostel-monitor--terminal-buffers-for-workspace key))
         (count (length bufs)))
    (when (> count 0)
      (list :count count
            :rss-kb 0))))

(defun hym-ghostel-monitor--badge (ws)
  "Status function: show terminal summary for workspace WS in the sidebar.
Wrapped in `condition-case' so a transient error (e.g. missing `ps',
killed buffer mid-iteration) never breaks the sidebar."
  (condition-case err
      (let* ((key (hym-workspace--key ws))
             (summary (or (gethash key hym-ghostel-monitor--sidebar-cache)
                          (hym-ghostel-monitor--cheap-summary key))))
        (hym-ghostel-monitor--schedule-sidebar-cache-refresh)
        (when-let ((line (hym-ghostel-monitor--summary-line summary)))
          (list line)))
    (error
     (message "ghostel-monitor badge error: %s" (error-message-string err))
     nil)))

;; ── Detail buffer ───────────────────────────────────────────────────────────

(defvar hym-ghostel-monitor-buffer-name "*ghostel-monitor*")

(defvar hym-ghostel-monitor--entries nil
  "Current list of terminal info plists for the detail buffer.")

(defun hym-ghostel-monitor--make-entry (info)
  "Return a `tabulated-list-entries' entry for INFO."
  (let* ((buf (plist-get info :buffer))
         (rss-kb (plist-get info :rss-kb))
         (marked (and (boundp 'hym-ghostel-monitor--marked)
                      (memq buf hym-ghostel-monitor--marked))))
    (list buf
          (vector
           (if marked
               (propertize "D" 'face 'error)
             " ")
           (number-to-string (plist-get info :pid))
           (propertize (hym-ghostel-monitor--format-memory rss-kb)
                       'sort-key rss-kb)
           (plist-get info :uptime)
           (plist-get info :state)
           (plist-get info :what)
           (plist-get info :buffer-name)
           (plist-get info :workspace)))))

(defun hym-ghostel-monitor--refresh-entries ()
  "Rebuild the entry list from live buffers."
  (setq tabulated-list-entries
        (mapcar #'hym-ghostel-monitor--make-entry
                (setq hym-ghostel-monitor--entries
                      (mapcar #'hym-ghostel-monitor--buffer-info
                              (hym-ghostel-monitor--terminal-buffers)))))
  (hym-ghostel-monitor--cache-sidebar-infos hym-ghostel-monitor--entries))

(defun hym-ghostel-monitor--entry-at-point ()
  "Return the info plist for the entry at point, or nil."
  (when-let ((buf (tabulated-list-get-id)))
    (when (bufferp buf)
      (seq-find (lambda (info) (eq (plist-get info :buffer) buf))
                hym-ghostel-monitor--entries))))

;; ── Kill helpers ────────────────────────────────────────────────────────────

(defun hym-ghostel-monitor--kill-entry (info)
  "Kill the terminal buffer described by INFO."
  (when (buffer-live-p (plist-get info :buffer))
    (let ((buf (plist-get info :buffer))
          (proc (get-buffer-process (plist-get info :buffer))))
      (when (and proc (process-live-p proc))
        (ignore-errors (kill-process proc)))
      (kill-buffer buf))))

;; ── Dired-style marking ─────────────────────────────────────────────────────

(defvar-local hym-ghostel-monitor--marked nil
  "List of buffer objects marked for deletion.")

(defun hym-ghostel-monitor--live-marked ()
  "Return live buffers currently marked for deletion."
  (setq hym-ghostel-monitor--marked
        (seq-filter #'buffer-live-p hym-ghostel-monitor--marked)))

(defun hym-ghostel-monitor--marked-infos ()
  "Return current info plists for marked live buffers."
  (let ((marked (hym-ghostel-monitor--live-marked)))
    (seq-filter
     (lambda (info)
       (memq (plist-get info :buffer) marked))
     hym-ghostel-monitor--entries)))

(defun hym-ghostel-monitor--marked-p (buf)
  "Return non-nil if BUF is marked for deletion."
  (memq buf hym-ghostel-monitor--marked))

(defun hym-ghostel-monitor-mark ()
  "Toggle the deletion mark on the current line."
  (interactive)
  (when-let ((buf (tabulated-list-get-id))
             ((bufferp buf)))
    (if (hym-ghostel-monitor--marked-p buf)
        (progn
          (setq hym-ghostel-monitor--marked
                (delete buf hym-ghostel-monitor--marked))
          (message "Unmarked"))
      (cl-pushnew buf hym-ghostel-monitor--marked :test #'eq)
      (message "Marked for deletion")))
  (when (tabulated-list-get-id)
    (forward-line 1))
  (hym-ghostel-monitor-refresh))

(defun hym-ghostel-monitor-unmark ()
  "Remove the deletion mark from the current line."
  (interactive)
  (when-let ((buf (tabulated-list-get-id))
             ((bufferp buf)))
    (setq hym-ghostel-monitor--marked
          (delete buf hym-ghostel-monitor--marked))
    (message "Unmarked")
    (hym-ghostel-monitor-refresh)))

(defun hym-ghostel-monitor-unmark-all ()
  "Remove all deletion marks."
  (interactive)
  (let ((n (length (hym-ghostel-monitor--live-marked))))
    (setq hym-ghostel-monitor--marked nil)
    (hym-ghostel-monitor-refresh)
    (message "%d mark%s removed" n (if (= n 1) "" "s"))))

(defun hym-ghostel-monitor-execute ()
  "Kill all marked terminals."
  (interactive)
  (hym-ghostel-monitor--refresh-entries)
  (let ((entries (hym-ghostel-monitor--marked-infos)))
    (if (null entries)
        (message "No terminals marked for deletion")
      (let ((count (length entries)))
        (when (yes-or-no-p
               (format "Kill %d marked terminal%s? "
                       count (if (> count 1) "s" "")))
          (dolist (info entries)
            (hym-ghostel-monitor--kill-entry info))
          (setq hym-ghostel-monitor--marked nil)
          (hym-ghostel-monitor-refresh)
          (hym-ghostel-monitor--sidebar-refresh)
          (message "Killed %d terminal%s" count (if (> count 1) "s" "")))))))

(defun hym-ghostel-monitor-kill-all ()
  "Kill all terminal buffers."
  (interactive)
  (hym-ghostel-monitor--refresh-entries)
  (let ((entries hym-ghostel-monitor--entries))
    (if (null entries)
        (message "No terminals found")
      (when (yes-or-no-p
             (format "Kill all %d terminal%s? "
                     (length entries) (if (> (length entries) 1) "s" "")))
        (dolist (info entries)
          (hym-ghostel-monitor--kill-entry info))
        (hym-ghostel-monitor-refresh)
        (hym-ghostel-monitor--sidebar-refresh)
        (message "Killed %d terminal%s"
                 (length entries) (if (> (length entries) 1) "s" ""))))))

(defun hym-ghostel-monitor--sidebar-refresh ()
  "Refresh the workspace sidebar if it exists."
  (when (fboundp 'hym-workspace-sidebar-refresh)
    (hym-workspace-sidebar-refresh)))

(defun hym-ghostel-monitor--safe-sort (a b)
  "Compare two entries by their Memory sort-key, defaulting to 0."
  (let ((key-a (or (ignore-errors
                     (get-text-property 0 'sort-key (aref (cadr a) 2)))
                   0))
        (key-b (or (ignore-errors
                     (get-text-property 0 'sort-key (aref (cadr b) 2)))
                   0)))
    (> key-a key-b)))

(defun hym-ghostel-monitor-refresh ()
  "Refresh the ghostel monitor buffer."
  (interactive)
  (let ((buf (get-buffer hym-ghostel-monitor-buffer-name)))
    (when buf
      (condition-case err
          (let ((win (get-buffer-window buf t)))
            (with-current-buffer buf
              (let ((saved-point (if win (window-point win) (point))))
                (hym-ghostel-monitor--refresh-entries)
                (tabulated-list-print t)
                (goto-char (min (if (number-or-marker-p saved-point)
                                    saved-point
                                  1)
                                (point-max)))
                (when (window-live-p win)
                  (set-window-point win (point))))))
        (error
         (message "ghostel-monitor refresh error: %s"
                  (error-message-string err)))))))

(defun hym-ghostel-monitor-visit ()
  "Visit the terminal buffer at point."
  (interactive)
  (when-let ((info (hym-ghostel-monitor--entry-at-point))
             (buf (plist-get info :buffer))
             ((buffer-live-p buf)))
    (switch-to-buffer-other-window buf)))

(defvar hym-ghostel-monitor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d")     #'hym-ghostel-monitor-mark)
    (define-key map (kbd "u")     #'hym-ghostel-monitor-unmark)
    (define-key map (kbd "U")     #'hym-ghostel-monitor-unmark-all)
    (define-key map (kbd "x")     #'hym-ghostel-monitor-execute)
    (define-key map (kbd "a")     #'hym-ghostel-monitor-kill-all)
    (define-key map (kbd "g")     #'hym-ghostel-monitor-refresh)
    (define-key map (kbd "q")     #'quit-window)
    (define-key map (kbd "RET")   #'hym-ghostel-monitor-visit)
    map)
  "Keymap for `hym-ghostel-monitor-mode'.")

(define-derived-mode hym-ghostel-monitor-mode tabulated-list-mode "Ghostel-Monitor"
  "Major mode for monitoring ghostel terminals.

Dired-style marking workflow:
  d     mark/unmark for deletion
  u     unmark current line
  U     unmark all
  x     kill all marked (with confirmation)
  a     kill all terminals
  g     refresh
  RET   visit terminal buffer
  q     quit"
  (setq tabulated-list-format
        [(""        1  nil)
         ("PID"      6  nil :right-align t)
         ("Memory"   10 hym-ghostel-monitor--safe-sort
          :pad-right 1)
         ("Uptime"   8  nil)
         ("State"    5  nil)
         ("Running"  10 nil)
         ("Buffer"   25 nil)
         ("Workspace" 15 nil)])
  (setq tabulated-list-padding 2)
  (setq truncate-lines t)
  (setq tabulated-list-sort-key '("Memory" . nil))
  (hym-ghostel-monitor--refresh-entries)
  (tabulated-list-init-header)
  (tabulated-list-print t)
  ;; Start in evil normal state so j/k navigate, and install local
  ;; keybindings that reliably override evil's global `d' operator.
  (when (fboundp 'evil-normal-state)
    (evil-normal-state 1))
  (when (fboundp 'evil-local-set-key)
    (evil-local-set-key 'normal (kbd "d")   #'hym-ghostel-monitor-mark)
    (evil-local-set-key 'normal (kbd "u")   #'hym-ghostel-monitor-unmark)
    (evil-local-set-key 'normal (kbd "U")   #'hym-ghostel-monitor-unmark-all)
    (evil-local-set-key 'normal (kbd "x")   #'hym-ghostel-monitor-execute)
    (evil-local-set-key 'normal (kbd "a")   #'hym-ghostel-monitor-kill-all)
    (evil-local-set-key 'normal (kbd "g")   #'hym-ghostel-monitor-refresh)
    (evil-local-set-key 'normal (kbd "q")   #'quit-window)
    (evil-local-set-key 'normal (kbd "RET") #'hym-ghostel-monitor-visit)))

;;;###autoload
(defun hym-ghostel-monitor ()
  "Open the ghostel terminal monitor buffer.
Lists every ghostel (and optionally vterm) terminal across all
workspaces with memory, uptime, and child process.

Dired-style marking:
  d     mark/unmark for deletion
  u     unmark current
  U     unmark all
  x     kill all marked (with confirmation)
  a     kill all terminals
  g     refresh
  RET   visit terminal buffer
  q     quit"
  (interactive)
  (if-let ((buf (get-buffer hym-ghostel-monitor-buffer-name)))
      (progn
        (pop-to-buffer buf)
        (hym-ghostel-monitor-refresh))
    (with-current-buffer (get-buffer-create hym-ghostel-monitor-buffer-name)
      (hym-ghostel-monitor-mode))
    (pop-to-buffer hym-ghostel-monitor-buffer-name)))

;; ── Initialisation ──────────────────────────────────────────────────────────

(defun hym-ghostel-monitor--install ()
  "Wire ghostel-monitor into the workspace sidebar and ghostel hooks."
  (condition-case err
      (progn
        ;; Tag ghostel buffers with their workspace.
        (with-eval-after-load 'ghostel
          (add-hook 'ghostel-mode-hook #'hym-ghostel-monitor--tag-buffer))

        ;; Also tag vterm buffers.
        (when hym-ghostel-monitor-track-vterm
          (with-eval-after-load 'vterm
            (add-hook 'vterm-mode-hook #'hym-ghostel-monitor--tag-buffer)))

        ;; Register the sidebar badge + keybinding (both regular and evil).
        (with-eval-after-load 'hym-workspaces-sidebar
          (add-to-list 'hym-workspace-sidebar-status-functions
                       #'hym-ghostel-monitor--badge)
          (when (bound-and-true-p hym-workspace-sidebar-mode-map)
            (define-key hym-workspace-sidebar-mode-map
                        (kbd "t") #'hym-ghostel-monitor)
            (when (fboundp 'evil-define-key)
              (evil-define-key 'normal hym-workspace-sidebar-mode-map
                (kbd "t") #'hym-ghostel-monitor)))))
    (error
     (message "ghostel-monitor install error: %s" (error-message-string err)))))

;; Install when loaded
(hym-ghostel-monitor--install)

(provide 'hym-ghostel-monitor)
;;; ghostel-monitor.el ends here

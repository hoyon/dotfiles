;; -*- lexical-binding: t -*-
(require 'seq)
(require 'tab-bar)

(defvar hym/default-tab-group "emacs"
  "Tab group used when a tab has no group assigned.")

(defun hym-tabs--apply-settings ()
  "Apply tab-bar settings used by `hym-tabs-mode'."
  (setq tab-bar-new-tab-group t
        tab-bar-new-tab-to 'right
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-hints t
        tab-bar-auto-width nil
        tab-bar-separator " "
        tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator)))

(define-minor-mode hym-tabs-mode
  "Enable custom grouped tab-bar behavior."
  :global t
  (if hym-tabs-mode
      (progn
        (hym-tabs--apply-settings)
        (tab-bar-mode 1)
        (add-hook 'emacs-startup-hook #'hym/ensure-tab-in-group)
        (add-hook 'after-make-frame-functions #'hym/ensure-tab-in-group)
        (add-hook 'server-after-make-frame-hook #'hym/ensure-tab-in-group)
        (advice-add 'tab-bar-new-tab :after #'hym/move-new-tab-to-group-end)
        (advice-add 'tab-bar-close-tab :around #'hym/tab-close-in-current-group)
        (advice-add 'tab-bar-select-tab :around #'hym/tab-select-tab-remember-group)
        (hym/ensure-tab-in-group))
    (remove-hook 'emacs-startup-hook #'hym/ensure-tab-in-group)
    (remove-hook 'after-make-frame-functions #'hym/ensure-tab-in-group)
    (remove-hook 'server-after-make-frame-hook #'hym/ensure-tab-in-group)
    (advice-remove 'tab-bar-new-tab #'hym/move-new-tab-to-group-end)
    (advice-remove 'tab-bar-close-tab #'hym/tab-close-in-current-group)
    (advice-remove 'tab-bar-select-tab #'hym/tab-select-tab-remember-group)))

(defun hym/ensure-tab-in-group (&optional frame)
  "Ensure the current tab on FRAME has a tab group, defaulting to `hym/default-tab-group'."
  (with-selected-frame (or frame (selected-frame))
    (unless (hym/tab-group)
      (tab-bar-change-tab-group hym/default-tab-group))))

(defvar hym/tab-group-last-tab (make-hash-table :test 'equal)
  "Map frame and group pairs to the stable ID of their last active tab.")

(defvar hym/tab-restore-group-selection t
  "Whether cross-group tab selection should restore the last active tab.")

(defun hym/tab-group (&optional tab)
  "Return the group of TAB, or the current tab when TAB is nil."
  (funcall tab-bar-tab-group-function (or tab (tab-bar--current-tab))))

(defun hym/tab-id (tab)
  "Return TAB's stable private ID, creating one when necessary."
  (or (alist-get 'hym-id tab)
      (let ((id (gensym "hym-tab-")))
        ;; Tab-bar preserves custom alist entries when switching tabs.
        (setcdr (last tab) (list (cons 'hym-id id)))
        id)))

(defun hym/tab-groups (&optional tabs)
  "Return group names in display order for TABS."
  (delete-dups
   (mapcar #'hym/tab-group (or tabs (funcall tab-bar-tabs-function)))))

(defun hym/tab-find-position (predicate &optional tabs)
  "Return the 1-based position of the first tab matching PREDICATE in TABS."
  (let ((position 1)
        found)
    (dolist (tab (or tabs (funcall tab-bar-tabs-function)) found)
      (when (and (null found) (funcall predicate tab))
        (setq found position))
      (setq position (1+ position)))))

(defun hym/tab-position-by-id (id &optional tabs)
  "Return the 1-based position of the tab with ID in TABS."
  (and id
       (hym/tab-find-position
        (lambda (tab) (eq (alist-get 'hym-id tab) id))
        tabs)))

(defun hym/tab-group-key (group)
  "Return the frame-local lookup key for GROUP."
  (cons (selected-frame) group))

(defun hym/tab-group-last-id (group)
  "Return the last active tab ID recorded for GROUP in this frame."
  (gethash (hym/tab-group-key group) hym/tab-group-last-tab))

(defun hym/tab-group-remember (group id)
  "Remember ID as the last active tab in GROUP for this frame."
  (puthash (hym/tab-group-key group) id hym/tab-group-last-tab))

(defun hym/tab-group-position (group &optional preferred-id tabs)
  "Return a position in GROUP, preferring the tab with PREFERRED-ID."
  (let ((tabs (or tabs (funcall tab-bar-tabs-function))))
    (or (and preferred-id
             (hym/tab-find-position
              (lambda (tab)
                (and (equal (hym/tab-group tab) group)
                     (eq (alist-get 'hym-id tab) preferred-id)))
              tabs))
        (hym/tab-find-position
         (lambda (tab) (equal (hym/tab-group tab) group))
         tabs))))

(defun hym/tab-group-save-current ()
  "Save the current tab as the last active tab for its group."
  (let ((current (tab-bar--current-tab-find)))
    (hym/tab-group-remember (hym/tab-group current) (hym/tab-id current))))

(defun hym/tab-group-switch-to (group)
  "Switch to GROUP, restoring the last active tab if possible."
  (hym/tab-group-save-current)
  (when-let ((position
              (hym/tab-group-position
               group (hym/tab-group-last-id group))))
    (let ((hym/tab-restore-group-selection nil))
      (tab-bar-select-tab position))))

(defun hym/tab-switch-to-group ()
  "Prompt for a tab group and switch to it."
  (interactive)
  (let* ((groups (hym/tab-groups))
         (group (completing-read "Switch to group: " groups nil t)))
    (hym/tab-group-switch-to group)))

(defun hym/tab-group-positions (group &optional tabs)
  "Return 1-based positions of tabs in GROUP within TABS."
  (let ((tabs (or tabs (funcall tab-bar-tabs-function)))
        (positions nil)
        (n 1))
    (dolist (tab tabs (nreverse positions))
      (when (equal (hym/tab-group tab) group)
        (push n positions))
      (setq n (1+ n)))))

;;; Cycle between tab groups
(defun hym/tab-cycle-group (direction)
  "Cycle to the next (DIRECTION=1) or previous (DIRECTION=-1) tab group."
  (let* ((current-group (hym/tab-group))
         (groups (hym/tab-groups))
         (pos (seq-position groups current-group #'string=))
         (next-pos (mod (+ pos direction) (length groups)))
         (next-group (nth next-pos groups)))
    (hym/tab-group-switch-to next-group)))

(defun hym/tab-switch-to-next-group ()
  "Switch to the first tab in the next tab group."
  (interactive)
  (hym/tab-cycle-group 1))

(defun hym/tab-switch-to-prev-group ()
  "Switch to the first tab in the previous tab group."
  (interactive)
  (hym/tab-cycle-group -1))

;;; Cycle within tab groups
(defun hym/tab-cycle-in-group (direction)
  "Cycle to the next (DIRECTION=1) or previous (DIRECTION=-1) tab in the current group."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (group-tab-nums (hym/tab-group-positions (hym/tab-group) tabs))
         (current-num (1+ (tab-bar--current-tab-index tabs)))
         (pos (seq-position group-tab-nums current-num))
         (next-pos (mod (+ pos direction) (length group-tab-nums))))
    (tab-bar-select-tab (nth next-pos group-tab-nums))))

(defun hym/tab-next-in-group ()
  "Switch to the next tab in the current tab group."
  (interactive)
  (hym/tab-cycle-in-group 1))

(defun hym/tab-previous-in-group ()
  "Switch to the previous tab in the current tab group."
  (interactive)
  (hym/tab-cycle-in-group -1))

(defun hym/tab-select-in-group (local-n)
  "Select the LOCAL-N'th tab in the current tab group."
  (interactive "nTab number: ")
  (let* ((positions (hym/tab-group-positions (hym/tab-group)))
         (global-pos (nth (1- local-n) positions)))
    (when global-pos
      (tab-bar-select-tab global-pos))))

(defun hym/tab-select-group (n)
  "Select the last active tab of the N'th tab group (1-based)."
  (interactive "nGroup number: ")
  (let ((group (nth (1- n) (hym/tab-groups))))
    (when group
      (hym/tab-group-switch-to group))))

(defun hym/tab-create (name)
  "Create a tab named NAME unless it already exists."
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))

(defun hym/tab-new-in-group ()
  "Create a new tab in a prompted tab group.
If the group exists, the tab is added to it. Otherwise a new group is created."
  (interactive)
  (let ((group (completing-read "Tab group: " (hym/tab-groups) nil nil)))
    (tab-new)
    (tab-bar-change-tab-group group)
    (hym/move-new-tab-to-group-end)))

(defun hym/tab-rename-group ()
  "Rename the current tab group."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-group (hym/tab-group))
         (new-name (read-string (format "Rename group '%s' to: " current-group)))
         (n 1))
    (dolist (tab tabs)
      (when (equal (hym/tab-group tab) current-group)
        (tab-bar-change-tab-group new-name n))
      (setq n (1+ n)))))

(defun hym/tab-move-to-group ()
  "Move the current tab to a different tab group.
If the group exists, the tab is moved into it. Otherwise a new group is created."
  (interactive)
  (let ((group (completing-read "Move to group: " (hym/tab-groups) nil nil)))
    (tab-bar-change-tab-group group)
    (hym/move-new-tab-to-group-end)))

(defun hym/tab-close-current-group ()
  "Close all tabs in the current tab group."
  (interactive)
  (tab-bar-close-group-tabs (hym/tab-group)))

(defun hym-tabs-setup-keybindings ()
  "Install keybindings for grouped tab-bar commands."
  (hym/leader-def
    "tj" 'hym/tab-switch-to-prev-group
    "tk" 'hym/tab-switch-to-next-group
    "tc" 'tab-close
    "tC" 'hym/tab-close-current-group
    "tr" 'tab-bar-rename-tab
    "tt" 'hym/tab-switch-to-group
    "tn" 'tab-new
    "tN" 'hym/tab-new-in-group
    "tR" 'hym/tab-rename-group
    "tm" 'hym/tab-move-to-group
    "t1" (lambda () (interactive) (hym/tab-select-in-group 1))
    "t2" (lambda () (interactive) (hym/tab-select-in-group 2))
    "t3" (lambda () (interactive) (hym/tab-select-in-group 3))
    "t4" (lambda () (interactive) (hym/tab-select-in-group 4))
    "t5" (lambda () (interactive) (hym/tab-select-in-group 5))
    "t6" (lambda () (interactive) (hym/tab-select-in-group 6))
    "t7" (lambda () (interactive) (hym/tab-select-in-group 7))
    "t8" (lambda () (interactive) (hym/tab-select-in-group 8))
    "t9" (lambda () (interactive) (hym/tab-select-in-group 9)))

  ;; Use cmd+number to change tab.
  (general-define-key
   "s-1" (lambda () (interactive) (hym/tab-select-in-group 1))
   "s-2" (lambda () (interactive) (hym/tab-select-in-group 2))
   "s-3" (lambda () (interactive) (hym/tab-select-in-group 3))
   "s-4" (lambda () (interactive) (hym/tab-select-in-group 4))
   "s-5" (lambda () (interactive) (hym/tab-select-in-group 5))
   "s-6" (lambda () (interactive) (hym/tab-select-in-group 6))
   "s-7" (lambda () (interactive) (hym/tab-select-in-group 7))
   "s-8" (lambda () (interactive) (hym/tab-select-in-group 8))
   "s-9" (lambda () (interactive) (hym/tab-select-in-group 9)))

  ;; Use SPC+number to switch tab group.
  (hym/leader-def
    "1" (lambda () (interactive) (hym/tab-select-group 1))
    "2" (lambda () (interactive) (hym/tab-select-group 2))
    "3" (lambda () (interactive) (hym/tab-select-group 3))
    "4" (lambda () (interactive) (hym/tab-select-group 4))
    "5" (lambda () (interactive) (hym/tab-select-group 5))
    "6" (lambda () (interactive) (hym/tab-select-group 6))
    "7" (lambda () (interactive) (hym/tab-select-group 7))
    "8" (lambda () (interactive) (hym/tab-select-group 8))
    "9" (lambda () (interactive) (hym/tab-select-group 9)))

  (general-define-key
   "C-<tab>" 'hym/tab-next-in-group
   "C-S-<iso-lefttab>" 'hym/tab-previous-in-group
   "C-<backtab>" 'hym/tab-previous-in-group))

(defun hym/move-new-tab-to-group-end (&rest _)
  "Move newly created tab to the end of its group."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current (seq-find (lambda (tab) (eq (car tab) 'current-tab)) tabs))
         (group (hym/tab-group current))
         (positions (hym/tab-group-positions group))
         (last-pos (car (last positions)))
         (current-pos (1+ (tab-bar--current-tab-index tabs))))
    (when (> last-pos current-pos)
      (tab-bar-move-tab-to last-pos))))

(defun hym/tab-close-in-current-group (orig-fn &optional tab-number to-number)
  "Keep selection in the current tab group after closing its current tab.
Use Emacs' normal close behavior when closing another tab, when TO-NUMBER
was supplied explicitly, or when the current tab is the last in its group."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-pos (1+ (tab-bar--current-tab-index tabs)))
         (closing-pos (or tab-number current-pos)))
    (if (or to-number (/= closing-pos current-pos))
        (funcall orig-fn tab-number to-number)
      (let* ((current-group (hym/tab-group))
             (closing-id (hym/tab-id (nth (1- closing-pos) tabs)))
             (positions (delq current-pos
                              (hym/tab-group-positions current-group tabs)))
             (next-pos (seq-find (lambda (pos) (> pos current-pos)) positions))
             (replacement (or next-pos (car (last positions))))
             (replacement-id
              (and replacement (hym/tab-id (nth (1- replacement) tabs)))))
        (prog1
            (let ((hym/tab-restore-group-selection nil))
              (funcall orig-fn tab-number replacement))
          (when (and replacement-id
                     (null (hym/tab-position-by-id closing-id)))
            (when-let ((replacement-pos
                        (hym/tab-position-by-id replacement-id)))
              (unless (eq (alist-get 'hym-id (tab-bar--current-tab))
                          replacement-id)
                (let ((hym/tab-restore-group-selection nil))
                  (tab-bar-select-tab replacement-pos)))
              (hym/tab-group-remember current-group replacement-id))))))))

(defun hym/tab-select-tab-remember-group (orig-fn &rest args)
  "Redirect cross-group tab switches to the last active tab in the target group."
  (if (not hym/tab-restore-group-selection)
      (apply orig-fn args)
    (hym/tab-group-save-current)
    (let* ((n (or (car args) (1+ (tab-bar--current-tab-index))))
           (tabs (funcall tab-bar-tabs-function))
           (target-tab (nth (1- n) tabs))
           (target-group (and target-tab (hym/tab-group target-tab))))
      (if (or (null target-tab)
              (equal target-group (hym/tab-group)))
          (apply orig-fn args)
        (funcall
         orig-fn
         (hym/tab-group-position
          target-group (hym/tab-group-last-id target-group) tabs))))))

(defun hym/tab-bar-mouse-close (event)
  "Close the group or tab clicked with mouse EVENT."
  (interactive "e")
  (let* ((item (tab-bar--event-to-item (event-start event)))
         (key (car item)))
    (if (and (symbolp key)
             (string-prefix-p "group-" (symbol-name key)))
        (let* ((tab-number
                (string-to-number
                 (substring (symbol-name key) (length "group-"))))
               (tab (nth (1- tab-number) (funcall tab-bar-tabs-function)))
               (group (and tab (hym/tab-group tab))))
          (when group
            (tab-bar-close-group-tabs group)))
      (tab-bar-mouse-close-tab event))))

(define-key tab-bar-map [down-mouse-2] #'hym/tab-bar-mouse-close)

(defun tab-bar-tab-name-format-hints (name tab i)
  (if tab-bar-tab-hints
      (let* ((positions (hym/tab-group-positions
                         (hym/tab-group tab)))
             (local-i (1+ (seq-position positions i))))
        (if (alist-get 'explicit-name tab)
            (format "«%d %s»" local-i name)
          (format "«%d»" local-i)))
    name))

(defun tab-bar-tab-group-format-default (tab _i &optional current-p)
  (let* ((group (hym/tab-group tab))
         (groups (hym/tab-groups))
         (group-n (1+ (seq-position groups group #'string=))))
    (propertize
     (format "┃%d %s┃" group-n group)
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)
     'help-echo "mouse-2: close this tab group")))

(when (and (fboundp 'hym/leader-def)
           (fboundp 'general-define-key))
  (hym-tabs-setup-keybindings))

(provide 'hym-tabs)

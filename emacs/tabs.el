;; -*- lexical-binding: t -*-

(tab-bar-mode 1)

(defvar hym/tab-group-last-tab (make-hash-table :test 'equal)
  "Maps group names to the last active tab name in that group.")

(defun hym/tab-group-save-current ()
  "Save the current tab as the last active tab for its group."
  (let* ((current (tab-bar--current-tab))
         (group (funcall tab-bar-tab-group-function current))
         (name (alist-get 'name current)))
    (puthash group name hym/tab-group-last-tab)))

(defun hym/tab-group-switch-to (group)
  "Switch to GROUP, restoring the last active tab if possible."
  (hym/tab-group-save-current)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (last-tab-name (gethash group hym/tab-group-last-tab))
         (n 1)
         (target nil)
         (fallback nil))
    (dolist (tab tabs)
      (when (string= (funcall tab-bar-tab-group-function tab) group)
        (unless fallback (setq fallback n))
        (when (and last-tab-name (string= (alist-get 'name tab) last-tab-name))
          (setq target n)))
      (setq n (1+ n)))
    (tab-bar-select-tab (or target fallback))))

(defun hym/tab-switch-to-group ()
  "Prompt for a tab group and switch to it."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (groups (delete-dups (mapcar (lambda (tab)
                                        (funcall tab-bar-tab-group-function tab))
                                      tabs)))
         (group (completing-read "Switch to group: " groups nil t)))
    (hym/tab-group-switch-to group)))

(defun hym/tab-group-positions (group)
  "Return the list of 1-based global tab positions belonging to GROUP."
  (let ((tabs (funcall tab-bar-tabs-function))
        (positions nil)
        (n 1))
    (dolist (tab tabs (nreverse positions))
      (when (string= (funcall tab-bar-tab-group-function tab) group)
        (push n positions))
      (setq n (1+ n)))))

;;; Cycle between tab groups
(defun hym/tab-cycle-group (direction)
  "Cycle to the next (DIRECTION=1) or previous (DIRECTION=-1) tab group."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-group (funcall tab-bar-tab-group-function (tab-bar--current-tab)))
         (groups (delete-dups (mapcar (lambda (tab)
                                        (funcall tab-bar-tab-group-function tab))
                                      tabs)))
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
         (current-group (funcall tab-bar-tab-group-function (tab-bar--current-tab)))
         (group-tab-nums nil)
         (current-num nil)
         (n 1))
    (dolist (tab tabs)
      (when (string= (funcall tab-bar-tab-group-function tab) current-group)
        (push n group-tab-nums)
        (when (eq (car tab) 'current-tab)
          (setq current-num n)))
      (setq n (1+ n)))
    (setq group-tab-nums (nreverse group-tab-nums))
    (let* ((pos (seq-position group-tab-nums current-num))
           (next-pos (mod (+ pos direction) (length group-tab-nums))))
      (tab-bar-select-tab (nth next-pos group-tab-nums)))))

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
  (let* ((positions (hym/tab-group-positions
                     (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
         (global-pos (nth (1- local-n) positions)))
    (when global-pos
      (tab-bar-select-tab global-pos))))

(defun hym/tab-select-group (n)
  "Select the last active tab of the N'th tab group (1-based)."
  (interactive "nGroup number: ")
  (let* ((tabs (funcall tab-bar-tabs-function))
         (groups (delete-dups (mapcar (lambda (tab)
                                        (funcall tab-bar-tab-group-function tab))
                                      tabs)))
         (group (nth (1- n) groups)))
    (when group
      (hym/tab-group-switch-to group))))

(defun hym/tab-create (name)
  "Creates a tab with the given name if it doens't exist."
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
  (let* ((tabs (funcall tab-bar-tabs-function))
         (groups (delete-dups (mapcar (lambda (tab)
                                        (funcall tab-bar-tab-group-function tab))
                                      tabs)))
         (group (completing-read "Tab group: " groups nil nil)))
    (tab-new)
    (tab-bar-change-tab-group group)
    (hym/move-new-tab-to-group-end)))

(defun hym/tab-rename-group ()
  "Rename the current tab group."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current-group (funcall tab-bar-tab-group-function (tab-bar--current-tab)))
         (new-name (read-string (format "Rename group '%s' to: " current-group)))
         (n 1))
    (dolist (tab tabs)
      (when (string= (funcall tab-bar-tab-group-function tab) current-group)
        (tab-bar-change-tab-group new-name n))
      (setq n (1+ n)))))

(defun hym/tab-move-to-group ()
  "Move the current tab to a different tab group.
If the group exists, the tab is moved into it. Otherwise a new group is created."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function))
         (groups (delete-dups (mapcar (lambda (tab)
                                        (funcall tab-bar-tab-group-function tab))
                                      tabs)))
         (group (completing-read "Move to group: " groups nil nil)))
    (tab-bar-change-tab-group group)
    (hym/move-new-tab-to-group-end)))

(hym/leader-def
  "tj" 'hym/tab-switch-to-prev-group
  "tk" 'hym/tab-switch-to-next-group
  "tc" 'tab-close
  "tr" 'tab-rename
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

;; Use cmd+number to change tab
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

;; Use spc+number to switch tab group
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
 "C-S-<iso-lefttab>" 'hym/tab-previous-in-group ;; Linux
 "C-<backtab>" 'hym/tab-previous-in-group) ;; macOS

(defun hym/move-new-tab-to-group-end (&rest _)
  "Move newly created tab to the end of its group."
  (let* ((tabs (funcall tab-bar-tabs-function))
         (current (seq-find (lambda (tab) (eq (car tab) 'current-tab)) tabs))
         (group (funcall tab-bar-tab-group-function current))
         (positions (hym/tab-group-positions group))
         (last-pos (car (last positions)))
         (current-pos (1+ (tab-bar--current-tab-index tabs))))
    (when (> last-pos current-pos)
      (tab-bar-move-tab-to last-pos))))

(advice-add 'tab-bar-new-tab :after #'hym/move-new-tab-to-group-end)

(defun hym/tab-select-tab-remember-group (orig-fn &rest args)
  "Redirect cross-group tab switches to the last active tab in the target group."
  (hym/tab-group-save-current)
  (let* ((n (or (car args) (1+ (tab-bar--current-tab-index))))
         (tabs (funcall tab-bar-tabs-function))
         (target-tab (nth (1- n) tabs)))
    (if (null target-tab)
        (apply orig-fn args)
      (let ((target-group (funcall tab-bar-tab-group-function target-tab))
            (current-group (funcall tab-bar-tab-group-function (tab-bar--current-tab))))
        (if (string= target-group current-group)
            (apply orig-fn args)
          (let ((last-tab-name (gethash target-group hym/tab-group-last-tab))
                (pos 1) (redirect nil) (fallback nil))
            (dolist (tab tabs)
              (when (string= (funcall tab-bar-tab-group-function tab) target-group)
                (unless fallback (setq fallback pos))
                (when (and last-tab-name (string= (alist-get 'name tab) last-tab-name))
                  (setq redirect pos)))
              (setq pos (1+ pos)))
            (funcall orig-fn (or redirect fallback))))))))

(advice-add 'tab-bar-select-tab :around #'hym/tab-select-tab-remember-group)

(setq tab-bar-new-tab-to 'right
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints t
      tab-bar-auto-width nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))

(defun tab-bar-tab-name-format-hints (name tab i)
  (if tab-bar-tab-hints
      (let* ((positions (hym/tab-group-positions
                         (funcall tab-bar-tab-group-function tab)))
             (local-i (1+ (seq-position positions i))))
        (format " %d " local-i))
    name))

(defun tab-bar-tab-group-format-default (tab _i &optional current-p)
  (let* ((group (funcall tab-bar-tab-group-function tab))
         (groups (delete-dups (mapcar (lambda (t_) (funcall tab-bar-tab-group-function t_))
                                      (funcall tab-bar-tabs-function))))
         (group-n (1+ (seq-position groups group #'string=))))
    (propertize
     (format "%d: %s" group-n group)
     'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive))))

;; (add-to-list 'display-buffer-alist
;; 			   '("\\*scratch\\*"
;; 				 (display-buffer-in-tab display-buffer-full-frame)
;; 				 (tab-group . "[EMACS]")))

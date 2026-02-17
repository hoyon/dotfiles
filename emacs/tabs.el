;; -*- lexical-binding: t -*-

(tab-bar-mode 1)

;; Stolen from https://www.rahuljuliato.com/posts/emacs-tab-bar-groups#
(defun hym/tab-switch-to-group ()
  "Prompt for a tab group and switch to its first tab.
Uses position instead of index field."
  (interactive)
  (let* ((tabs (funcall tab-bar-tabs-function)))
    (let* ((groups (delete-dups (mapcar (lambda (tab)
                                          (funcall tab-bar-tab-group-function tab))
                                        tabs)))
           (group (completing-read "Switch to group: " groups nil t)))
      (let ((i 1) (found nil))
        (dolist (tab tabs)
          (let ((tab-group (funcall tab-bar-tab-group-function tab)))
            (when (and (not found)
                       (string= tab-group group))
              (setq found t)
              (tab-bar-select-tab i)))
          (setq i (1+ i)))))))

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

(defun hym/tab-create (name)
  "Creates a tab with the given name if it doens't exist."
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))

(hym/leader-def
  "tj" 'tab-previous
  "tk" 'tab-next
  "tc" 'tab-close
  "tr" 'tab-rename
  "tt" 'hym/tab-switch-to-group
  "tn" 'tab-new
  "t1" (lambda () (interactive) (tab-select 1))
  "t2" (lambda () (interactive) (tab-select 2))
  "t3" (lambda () (interactive) (tab-select 3))
  "t4" (lambda () (interactive) (tab-select 4))
  "t5" (lambda () (interactive) (tab-select 5))
  "t6" (lambda () (interactive) (tab-select 6))
  "t7" (lambda () (interactive) (tab-select 7))
  "t8" (lambda () (interactive) (tab-select 8))
  "t9" (lambda () (interactive) (tab-select 9))
  )

;; Use cmd+number to change tab
(keymap-global-set "s-1" (lambda () (interactive) (tab-select 1)))
(keymap-global-set "s-2" (lambda () (interactive) (tab-select 2)))
(keymap-global-set "s-3" (lambda () (interactive) (tab-select 3)))
(keymap-global-set "s-4" (lambda () (interactive) (tab-select 4)))
(keymap-global-set "s-5" (lambda () (interactive) (tab-select 5)))
(keymap-global-set "s-6" (lambda () (interactive) (tab-select 6)))
(keymap-global-set "s-7" (lambda () (interactive) (tab-select 7)))
(keymap-global-set "s-8" (lambda () (interactive) (tab-select 8)))
(keymap-global-set "s-9" (lambda () (interactive) (tab-select 9)))

(keymap-global-set "C-<tab>" #'hym/tab-next-in-group)
(keymap-global-set "C-S-<iso-lefttab>" #'hym/tab-previous-in-group) ;; Linux
(keymap-global-set "C-<backtab>" #'hym/tab-previous-in-group) ;; macOS

(setq tab-bar-new-tab-to 'right
      tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-hints t
      tab-bar-auto-width nil
      tab-bar-separator " "
      tab-bar-format '(tab-bar-format-tabs-groups tab-bar-separator))

(defun tab-bar-tab-name-format-hints (name _tab i)
  (if tab-bar-tab-hints (concat (format " %d " i) "") name))

(defun tab-bar-tab-group-format-default (tab _i &optional current-p)
  (propertize
   (concat (funcall tab-bar-tab-group-function tab))
   'face (if current-p 'tab-bar-tab-group-current 'tab-bar-tab-group-inactive)))

;; (add-to-list 'display-buffer-alist
;; 			   '("\\*scratch\\*"
;; 				 (display-buffer-in-tab display-buffer-full-frame)
;; 				 (tab-group . "[EMACS]")))

;; -*- lexical-binding: t -*-

(tab-bar-mode 1)

(setq tab-bar-new-tab-to 'rightmost)

(defun hym/tab-bar-tab-name-format (tab i)
  "Format TAB with index I to include the tab number."
  (propertize
   (concat (number-to-string i) " " (alist-get 'name tab))
   'face (funcall tab-bar-tab-face-function tab)))

(setq tab-bar-tab-name-format-function #'hym/tab-bar-tab-name-format)

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
  "tt" 'tab-switch
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

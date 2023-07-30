;; -*- lexical-binding: t -*-

(tab-bar-mode 1)

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
  "ta" 'tab-switch
  "tn" 'tab-new
  "t1" (lambda () (interactive) (tab-select 1))
  "t2" (lambda () (interactive) (tab-select 2))
  "t3" (lambda () (interactive) (tab-select 3))
  "t4" (lambda () (interactive) (tab-select 4))
  "t5" (lambda () (interactive) (tab-select 5))
  )

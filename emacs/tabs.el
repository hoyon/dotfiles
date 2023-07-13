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
  "aj" 'tab-previous
  "ak" 'tab-next
  "ac" 'tab-close
  "ar" 'tab-rename
  "aa" 'tab-switcher
  "an" 'tab-new
  "a1" (lambda () (interactive) (tab-select 1))
  "a2" (lambda () (interactive) (tab-select 2))
  "a3" (lambda () (interactive) (tab-select 3))
  "a4" (lambda () (interactive) (tab-select 4))
  "a5" (lambda () (interactive) (tab-select 5))
  )

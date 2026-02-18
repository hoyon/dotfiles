;; -*- lexical-binding: t -*-
;; Inspired by this guide: https://www.labri.fr/perso/nrougier/GTD/index.html

(setq-default fill-column 90)

(setq hym/weekly-dir "~/org/weekly")

(setq org-directory "~/org"
      org-agenda-files '("~/org" "~/org/weekly")
      org-archive-location "~/org/archive/%s_archive::datetree/"
      org-export-backends '(ascii html icalendar latex odt md)

      ;; Startup options
      org-startup-folded 'showeverything
      org-startup-indented 'indent
      org-hide-leading-stars 'showstars
      org-cycle-separator-lines 1)

(defun hym/create-meeting-file ()
  "Create a meeting file"
  (interactive)
  (let* ((meeting-name (read-string "Meeting name: "))
         (date (format-time-string "%F" (current-time)))
         (filename (expand-file-name (format "%s-%s.org" (string-replace " " "-" meeting-name) date) "~/org/meetings")))
    (find-file-other-window filename)
    (insert "#+FILETAGS: meeting")
    (newline)
    (newline)))

(defun hym/ordinal-suffix (time)
  "returns st, nd or th for the day"
  (let ((day (format-time-string "%d" time)))
    (cond
     ((member day '("11" "12" "13")) "th")
     ((string-match-p "1$" day) "st")
     ((string-match-p "2$" day) "nd")
     ((string-match-p "3$" day) "rd")
     (t "th"))))

(defun hym/get-monday (time)
  "returns the monday of that week"
  (let* ((d (decode-time time))
         (days-back (thread-first d (decoded-time-weekday) (- 1) (mod 7) -)))
    (encode-time (decoded-time-add d (make-decoded-time :day days-back)))))

(defun hym/org-has-heading (heading)
  "returns non-nil if current buffer has an org header"
  (memq t (org-map-entries
           (lambda ()
             (message (substring-no-properties (org-get-heading t t)))
             (when (string= (substring-no-properties (org-get-heading t t)) heading)
               t))
           nil 'file)))

(defun hym/open-weekly-note ()
  "Open the note for the current week at today's date"
  (interactive)
  (let* ((now (current-time))
         (monday (hym/get-monday now))
         (filename (expand-file-name (format-time-string "%F.org" monday) hym/weekly-dir))
         (today (format-time-string (concat "%A %-d" (hym/ordinal-suffix now) " %B") now)))

    (unless (file-exists-p hym/weekly-dir)
      (make-directory hym/weekly-dir t))
    (let ((buf (or (get-file-buffer filename)
                   (find-file-noselect filename))))
      (message (concat "opening " filename "..."))
      (switch-to-buffer buf)
      (unless (file-exists-p filename)
        (write-region "" nil filename))
      (org-show-all)

      (unless (hym/org-has-heading today)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (concat "* " today "\n"))))))

(setq org-capture-templates
      `(("i" "Inbox" entry (file+headline "inbox.org" "Inbox")
         ,(concat "** TODO %?\n"))))

(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
        ("work.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "WONTDO(w)" "MIGRATED(m)")))

(setq org-log-done 'time)

(setq org-agenda-custom-commands
      '(("g" "Get Things Done (GTD)"
         ((agenda ""
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-deadline-warning-days 0)))
          (todo "NEXT"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'deadline))
                 (org-agenda-prefix-format "  %i %-12:c [%e] ")
                 (org-agenda-overriding-header "\nTasks\n")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline))
                   (org-agenda-format-date "")
                   (org-deadline-warning-days 7)
                   ;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header "\nDeadlines")))
          (tags-todo "weekly"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nWeekly\n")))
          (tags-todo "inbox"
                     ((org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-overriding-header "\nInbox\n")))
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-overriding-header "\nCompleted today\n")))))))

(defun hym/org-capture-inbox ()
  (interactive)
  (ignore-errors
    (call-interactively 'org-store-link))
  (org-capture nil "i"))

(defun hym/find-org ()
  (interactive)
  (consult-fd "~/org" "\\.org$ "))

(defun hym/open-org-config ()
  (interactive)
  (find-file (concat user-emacs-directory "org.el")))

(defun hym/open-inbox ()
  (interactive)
  (find-file "~/org/inbox.org"))

(hym/leader-def
  "o." 'hym/open-org-config
  "oo" 'hym/open-inbox
  "oc" 'org-capture
  "oa" 'org-agenda
  "oi" 'hym/org-capture-inbox
  "om" 'hym/create-meeting-file
  "of" 'hym/find-org
  "oA" 'org-archive-subtree
  "ow" 'hym/open-weekly-note)

(load-config "org-uk-holidays.el")

(defun hym/save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
             (when (member (buffer-file-name) org-agenda-files)
               t)))
  (message "Saving org-agenda-files buffers... done"))

(advice-add 'org-refile :after
        (lambda (&rest _)
          (hym/save-org-buffers)))

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-modern-star 'replace))

(use-package evil-org
  :after org
  :delight
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (add-hook 'org-mode-hook #'evil-org-mode))

(add-hook 'org-mode-hook #'auto-revert-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)

(use-package unfill)

(use-package verb
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (add-to-list 'verb-content-type-handlers '("application/hal\\+json" verb-handler-json)))

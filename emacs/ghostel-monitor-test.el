;; -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(load-file (expand-file-name "ghostel-monitor.el" (file-name-directory load-file-name)))

(unless (fboundp 'ghostel-mode)
  (define-derived-mode ghostel-mode special-mode "Ghostel"))

(defmacro hym-ghostel-monitor-test-with-buffers (buffers &rest body)
  "Create ghostel BUFFERS and run BODY, cleaning up afterwards."
  (declare (indent 1) (debug t))
  `(let ((hym-ghostel-monitor-track-vterm nil)
         (hym-ghostel-monitor-buffer-name
          (generate-new-buffer-name "*ghostel-monitor-test*"))
         created)
     (unwind-protect
         (let ,(mapcar (lambda (name)
                         `(,name (let ((buf (generate-new-buffer
                                             ,(format " *%s*" name))))
                                   (push buf created)
                                   (with-current-buffer buf
                                     (ghostel-mode))
                                   buf)))
                       buffers)
           ,@body)
       (dolist (buf created)
         (when (buffer-live-p buf)
           (kill-buffer buf)))
       (when-let ((buf (get-buffer hym-ghostel-monitor-buffer-name)))
         (kill-buffer buf)))))

(ert-deftest hym-ghostel-monitor-refresh-preserves-monitor-point ()
  (hym-ghostel-monitor-test-with-buffers (one two)
    (with-current-buffer (get-buffer-create hym-ghostel-monitor-buffer-name)
      (hym-ghostel-monitor-mode)
      (goto-char (point-max)))
    (let ((saved-point (with-current-buffer hym-ghostel-monitor-buffer-name
                         (point))))
      (with-temp-buffer
        (goto-char (point-min))
        (hym-ghostel-monitor-refresh))
      (with-current-buffer hym-ghostel-monitor-buffer-name
        (should (= (point) saved-point))))))

(ert-deftest hym-ghostel-monitor-detail-filters-by-workspace ()
  (hym-ghostel-monitor-test-with-buffers (one two)
    (with-current-buffer one
      (setq hym-ghostel-monitor--workspace "one"))
    (with-current-buffer two
      (setq hym-ghostel-monitor--workspace "two"))
    (let ((hym-ghostel-monitor--initial-workspace-filter "one")
          (hym-ghostel-monitor--sidebar-cache
           (make-hash-table :test 'equal)))
      (cl-letf (((symbol-function 'hym-ghostel-monitor--capture-process-table)
                 (lambda () (make-hash-table :test 'eql))))
        (with-current-buffer
            (get-buffer-create hym-ghostel-monitor-buffer-name)
          (hym-ghostel-monitor-mode)
          (should (equal hym-ghostel-monitor--workspace-filter "one"))
          (should (equal hym-ghostel-monitor--entries
                         (list (hym-ghostel-monitor--buffer-info
                                one (make-hash-table :test 'eql)))))
          ;; Filtering the detail view must not truncate the global cache.
          (should (gethash "one" hym-ghostel-monitor--sidebar-cache))
          (should (gethash "two" hym-ghostel-monitor--sidebar-cache)))))))

(ert-deftest hym-ghostel-monitor-sidebar-workspace-uses-workspace-key ()
  (let (opened-with)
    (cl-letf (((symbol-function 'derived-mode-p)
               (lambda (&rest _) t))
              ((symbol-function 'hym-workspace-sidebar--at-point)
               (lambda () "action gate"))
              ((symbol-function 'hym-workspace-get)
               (lambda (name)
                 (and (equal name "action gate")
                      '(:name "action gate" :slug "action_gate"))))
              ((symbol-function 'hym-ghostel-monitor)
               (lambda (workspace)
                 (setq opened-with workspace))))
      (hym-ghostel-monitor-sidebar-workspace)
      (should (equal opened-with "action_gate")))))

(ert-deftest hym-ghostel-monitor-live-marked-prunes-dead-buffers ()
  (hym-ghostel-monitor-test-with-buffers (live dead)
    (with-current-buffer (get-buffer-create hym-ghostel-monitor-buffer-name)
      (hym-ghostel-monitor-mode)
      (setq hym-ghostel-monitor--marked (list live dead))
      (kill-buffer dead)
      (should (equal (hym-ghostel-monitor--live-marked) (list live))))))

(ert-deftest hym-ghostel-monitor-badge-does-not-scan-processes ()
  (hym-ghostel-monitor-test-with-buffers (term)
    (with-current-buffer term
      (setq hym-ghostel-monitor--workspace "ws"))
    (let ((hym-ghostel-monitor--sidebar-cache (make-hash-table :test 'equal))
          (hym-ghostel-monitor--sidebar-cache-time (float-time)))
      (cl-letf (((symbol-function 'hym-workspace--key)
                 (lambda (_) "ws"))
                ((symbol-function 'hym-ghostel-monitor--buffer-info)
                 (lambda (_) (error "expensive scan during badge render"))))
        (should (equal (hym-ghostel-monitor--badge '(:name "ws"))
                       '("▸ 1 term · 0 KB")))))))

(ert-deftest hym-ghostel-monitor-parses-macos-process-snapshot ()
  (let* ((text (concat
                "    1     0  13744 Ss   10-00:19:44 /sbin/launchd\n"
                "  100     1   2048 S       01:02:03 /opt/homebrew/bin/fish\n"
                "  101   100   4096 R          00:05 /usr/local/bin/claude\n"))
         (table (hym-ghostel-monitor--parse-process-table text))
         (root (gethash 1 table))
         (shell (gethash 100 table)))
    (should (= (hash-table-count table) 3))
    (should (equal (plist-get root :comm) "launchd"))
    (should (equal (plist-get shell :comm) "fish"))
    (should (= (plist-get root :etime) 865184))
    (should (= (plist-get shell :etime) 3723))
    (should (equal (plist-get shell :children) '(101)))
    (should (= (hym-ghostel-monitor--snapshot-tree-rss 100 table) 6144))
    (should (equal
             (hym-ghostel-monitor--snapshot-interesting-child 100 table)
             "claude"))))

(ert-deftest hym-ghostel-monitor-parses-linux-process-snapshot ()
  (let* ((text (concat
                "      1       0  12300 Ss   2-03:04:05 systemd\n"
                "   2000       1   1500 S          09:08 bash\n"
                "   2001    2000  25000 Sl+        01:07 node\n"))
         (table (hym-ghostel-monitor--parse-process-table text))
         (root (gethash 1 table))
         (node (gethash 2001 table)))
    (should (= (hash-table-count table) 3))
    (should (equal (plist-get root :comm) "systemd"))
    (should (= (plist-get root :etime) 183845))
    (should (equal (plist-get node :state) "Sl+"))
    (should (= (plist-get node :etime) 67))
    (should (= (hym-ghostel-monitor--snapshot-tree-rss 2000 table) 26500))
    (should (equal
             (hym-ghostel-monitor--snapshot-interesting-child 2000 table)
             "node"))))

(ert-deftest hym-ghostel-monitor-process-tree-rss-guards-cycles ()
  (let ((table (make-hash-table :test 'eql)))
    (puthash 1 '(:rss-kb 1 :children (2)) table)
    (puthash 2 '(:rss-kb 1 :children (1)) table)
    (should (= (hym-ghostel-monitor--snapshot-tree-rss 1 table) 2))))

(ert-deftest hym-ghostel-monitor-buffer-info-reuses-process-snapshot ()
  (hym-ghostel-monitor-test-with-buffers (term)
    (with-current-buffer term
      (setq-local ghostel--pid 100))
    (let ((table (hym-ghostel-monitor--parse-process-table
                  (concat
                   "  100     1   2048 S       01:02:03 /bin/fish\n"
                   "  101   100   4096 R          00:05 /usr/bin/claude\n"))))
      (cl-letf (((symbol-function 'hym-ghostel-monitor--capture-process-table)
                 (lambda () (error "captured a second process snapshot"))))
        (let ((info (hym-ghostel-monitor--buffer-info term table)))
          (should (= (plist-get info :rss-kb) 6144))
          (should (equal (plist-get info :what) "claude"))
          (should (equal (plist-get info :uptime) "1h 2m")))))))

(ert-deftest hym-ghostel-monitor-sidebar-refresh-starts-one-async-scan ()
  (let ((hym-ghostel-monitor--sidebar-cache-process nil)
        (hym-ghostel-monitor--sidebar-cache-timer 'stale-timer)
        (starts 0)
        process)
    (unwind-protect
        (cl-letf (((symbol-function 'hym-ghostel-monitor--terminal-buffers)
                   (lambda () nil))
                  ((symbol-function 'hym-ghostel-monitor--make-process)
                   (lambda (&rest args)
                     (setq starts (1+ starts))
                     (setq process
                           (make-pipe-process
                            :name "ghostel-monitor-test-ps"
                            :buffer (plist-get args :buffer)
                            :noquery t)))))
          (hym-ghostel-monitor--refresh-sidebar-cache)
          (should (eq hym-ghostel-monitor--sidebar-cache-timer nil))
          (should (process-live-p
                   hym-ghostel-monitor--sidebar-cache-process))
          ;; A live scan suppresses overlapping refresh processes.
          (hym-ghostel-monitor--refresh-sidebar-cache)
          (should (= starts 1)))
      (when (process-live-p process)
        (delete-process process))
      (when process
        (when-let ((stdout (process-buffer process)))
          (when (buffer-live-p stdout) (kill-buffer stdout)))
        (when-let ((stderr
                    (process-get process 'hym-ghostel-monitor-stderr)))
          (when (buffer-live-p stderr) (kill-buffer stderr)))))))

(ert-deftest hym-ghostel-monitor-buffer-pid-falls-back-to-ghostel-pid ()
  (hym-ghostel-monitor-test-with-buffers (term)
    (with-current-buffer term
      (setq-local ghostel--pid 12345))
    (should (= (hym-ghostel-monitor--buffer-pid term) 12345))))

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

(ert-deftest hym-ghostel-monitor-process-tree-rss-guards-cycles ()
  (cl-letf (((symbol-function 'hym-ghostel-monitor--process-rss-kb)
             (lambda (_) 1))
            ((symbol-function 'hym-ghostel-monitor--process-children)
             (lambda (pid)
               (pcase pid
                 (1 '(2))
                 (2 '(1))
                 (_ nil)))))
    (should (= (hym-ghostel-monitor--process-tree-rss 1) 2))))

(ert-deftest hym-ghostel-monitor-buffer-pid-falls-back-to-ghostel-pid ()
  (hym-ghostel-monitor-test-with-buffers (term)
    (with-current-buffer term
      (setq-local ghostel--pid 12345))
    (cl-letf (((symbol-function 'get-buffer-process)
               (lambda (_) nil)))
      (should (= (hym-ghostel-monitor--buffer-pid term) 12345)))))

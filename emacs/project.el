;; -*- lexical-binding: t -*-

(add-to-list 'safe-local-variable-values '(hym/project-compile-command . "./build"))
(add-to-list 'safe-local-variable-values '(hym/project-run-command . "./build --run"))

(defun hym/project-compile ()
  "Compile project in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (if (boundp 'hym/project-compile-command)
        (compile hym/project-compile-command)
      (call-interactively #'compile))))

(defun hym/project-run ()
  "Run project in the project root."
  (declare (interactive-only compile))
  (interactive)
  (if (boundp 'hym/project-run-command)
      (let ((default-directory (project-root (project-current t)))
            (compilation-buffer-name-function
             (or project-compilation-buffer-name-function
                 compilation-buffer-name-function)))
        (compile hym/project-run-command))
    (message "hym/project-run-command not set!")))

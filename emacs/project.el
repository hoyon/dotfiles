;; -*- lexical-binding: t -*-

;; config

; disable dispatch menu
(setq project-switch-commands 'project-find-file)

;; project compile shortcuts

(defvar hym/project-compile-command "")
(defvar hym/project-run-command "")
(defvar hym/project-test-command "")

(put 'hym/project-compile-command 'safe-local-variable #'stringp)
(put 'hym/project-run-command 'safe-local-variable #'stringp)
(put 'hym/project-test-command 'safe-local-variable #'stringp)

(defun hym/project-compile ()
  "Compile project in the project root."
  (declare (interactive-only compile))
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function)))
    (if (not (string= "" hym/project-compile-command))
        (compile hym/project-compile-command)
      (call-interactively #'compile))))

(defun hym/project-run ()
  "Run project in the project root."
  (declare (interactive-only compile))
  (interactive)
  (if (not (string= "" hym/project-run-command))
      (let ((default-directory (project-root (project-current t)))
            (compilation-buffer-name-function
             (or project-compilation-buffer-name-function
                 compilation-buffer-name-function)))
        (compile hym/project-run-command))
    (message "hym/project-run-command not set!")))

(defun hym/project-test ()
  "Test project in the project root."
  (declare (interactive-only compile))
  (interactive)
  (if (not (string= "" hym/project-test-command))
      (let ((default-directory (project-root (project-current t)))
            (compilation-buffer-name-function
             (or project-compilation-buffer-name-function
                 compilation-buffer-name-function)))
        (compile hym/project-test-command))
    (message "hym/project-test-command not set!")))

(defun hym/project-find-file-all ()
  "project-find-file including gitignore"
  (declare (interactive-only compile))
  (interactive)
  (project-find-file 't))

;; bindings

(hym/leader-def
  "pp" 'project-switch-project
  "pf" 'project-find-file
  "pF" 'hym/project-find-file-all
  "p/" 'consult-ripgrep
  "pc" 'hym/project-compile
  "pr" 'hym/project-run
  "pt" 'hym/project-test
  "p&" 'project-async-shell-command
  "p!" 'project-shell-command
  "pe" 'project-eshell
  "pv" 'vterm)

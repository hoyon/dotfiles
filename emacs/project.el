;; -*- lexical-binding: t -*-

;; config

; disable dispatch menu
(setq project-switch-commands 'project-find-file)

;; project compile shortcuts

(defvar hym/project-compile-command "")
(defvar hym/project-run-command "")

(put 'hym/project-compile-command 'safe-local-variable #'stringp)
(put 'hym/project-run-command 'safe-local-variable #'stringp)

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

;; bindings

(hym/leader-def
  "pp" 'project-switch-project
  "pf" 'project-find-file
  "p/" 'consult-ripgrep
  "pc" 'hym/project-compile
  "pr" 'hym/project-run
  "p&" 'project-async-shell-command
  "p!" 'project-shell-command
  "pe" 'project-eshell
  "pv" 'vterm)

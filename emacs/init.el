;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(add-hook 'focus-out-hook 'garbage-collect)

(setq comp-async-report-warning-errors nil
      load-prefer-newer t)

(defvar bootstrap-version)
(setq straight-use-package-by-default 't
      straight-check-for-modifications nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq column-number-mode t
      scroll-conservatively 10)

(setq frame-title-format
      (list (format "%%F - %%j")
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(defun load-config (filename)
  "Load config file"
  (load (expand-file-name filename user-emacs-directory)))

(setq
 sentence-end-double-space nil ;; Don't require double spaces to separate spaces. Affects M-q
 use-short-answers 't ;; yes-or-no-p -> y-or-n-p
 ring-bell-function 'ignore ;; disable audible bell
 next-error-message-highlight 'keep)

(save-place-mode 1)

(setq shell-file-name "/bin/bash")

;; Move autosave and backup files to reduce clutter and disable lock files
(make-directory (format "%sautosave/" user-emacs-directory) t)

(setq
 auto-save-file-name-transforms `((".*" ,(format "%sautosave/" user-emacs-directory) t))
 backup-directory-alist `(("" . ,(format "%sbackup" user-emacs-directory)))
 create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(advice-add 'c-update-modeline :around #'ignore)

(load "server")
(unless (server-running-p) (server-start))

(load-config "evil.el")

(use-package general
  :config
  (general-create-definer hym/leader-def
    :prefix "SPC"
    :states 'normal
    :keymaps 'override)
  (general-create-definer hym/local-leader-def
    :prefix ","
    :states 'normal
    :keymaps 'local))

(load-config "theme.el")
(load-config "vertico.el")
(load-config "shell.el")
(load-config "git.el")
(load-config "window.el")

(defun hym/grep-for-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (symbol-name (symbol-at-point))))

(defun hym/copy-buffer-file-name ()
  (interactive)
  (if-let ((file-name (buffer-file-name)))
      (progn
        (message file-name)
        (kill-new file-name))
    (error "Buffer not visiting a file")))

(defun hym/delete-current-file ()
  (interactive)
  (if-let ((file-name (buffer-file-name))
           (p (yes-or-no-p (concat "Delete " file-name "? "))))
      (progn
        (delete-file file-name)
        (kill-buffer))
    (error "Buffer not visiting a file")))

(defun hym/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun hym/search-in-directory ()
  "Prompts for directory and does a search there"
  (interactive)
  (let ((dir (read-directory-name "Dir to search: ")))
    (consult-ripgrep dir)))

(hym/leader-def
  ":" 'execute-extended-command
  "," 'consult-buffer
  "<" 'consult-project-extra-find
  "SPC" 'project-find-file
  "fs" 'evil-write
  "fy" 'hym/copy-buffer-file-name
  "fd" 'hym/delete-current-file
  "fr" 'hym/rename-current-buffer-file
  "br" 'revert-buffer
  "pp" 'project-switch-project
  "pf" 'project-find-file
  "p/" 'consult-ripgrep
  "pc" 'project-compile
  "p&" 'project-async-shell-command
  "p!" 'project-shell-command
  "pe" 'project-eshell
  "pv" 'vterm
  "*"  'hym/grep-for-symbol-at-point
  "tl" 'global-display-line-numbers-mode
  "tf" 'hym/toggle-font-size
  "sd" 'hym/search-in-directory
  "sl" 'consult-line
  "si" 'consult-imenu
  "sI" 'consult-imenu-multi
  "hi" 'info
  "hm" 'man)

(load-config "lang.el")
(load-config "org.el")

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (hym/leader-def
    "hf" 'helpful-callable
    "hv" 'helpful-variable
    "hk" 'helpful-key))

(use-package delight
  :config
  (delight '((evil-collection-unimpaired-mode nil "evil-collection-unimpaired")
             (cargo-minor-mode nil "cargo")
             (eldoc-mode nil "eldoc")
             (yas-minor-mode nil "yasnippet")
             (auto-revert-mode nil "autorevert")
             (auto-fill-function nil t)
             (abbrev-mode nil "abbrev")
             (evil-org-mode nil "evil-org")
             (org-indent-mode nil "org"))))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(if (boundp 'minibuffer-mode-map)
    (define-key minibuffer-mode-map (kbd "C-S-v") 'yank))

(use-package dired+)
(use-package info+)
(use-package docker)

;; Make tramp work with guix
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; Disable some tramp messages
(setq-default tramp-verbose 2)

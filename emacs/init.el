;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(add-hook 'focus-out-hook 'garbage-collect)

(setq comp-async-report-warning-errors nil
      native-comp-deferred-compilation t
      native-compile-prune-cache t
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

(setq column-number-mode t
      scroll-conservatively 10)

(setq frame-title-format
      (list (format "%%F - %%j")
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(add-to-list 'default-frame-alist '(height . 60))
(add-to-list 'default-frame-alist '(width . 180))

(defun load-config (filename)
  "Load config file"
  (load (expand-file-name filename user-emacs-directory)))

(setq
 sentence-end-double-space nil ;; Don't require double spaces to separate spaces. Affects M-q
 use-short-answers 't ;; yes-or-no-p -> y-or-n-p
 ring-bell-function 'ignore ;; disable audible bell
 next-error-message-highlight 'keep)

;; Store autosave and backup files in ~/.local/share/emacs
(defvar hym/emacs-local-dir (expand-file-name "~/.local/share/emacs/"))
(make-directory (concat hym/emacs-local-dir "autosave/") t)
(make-directory (concat hym/emacs-local-dir "backup/") t)

(setq
 auto-save-file-name-transforms `((".*" ,(concat hym/emacs-local-dir "autosave/") t))
 backup-directory-alist `(("" . ,(concat hym/emacs-local-dir "backup/")))
 create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(advice-add 'c-update-modeline :around #'ignore)

(load "server")
(unless (server-running-p) (server-start))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package compat)
(use-package delight)
(use-package emacs
  :delight
  (eldoc-mode)
  (auto-fill-mode)
  (auto-revert-mode)
  (hs-minor-mode))

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
(load-config "project.el")
(load-config "docs.el")
(load-config "lang.el")
(load-config "completion.el")
(load-config "compile.el")
(load-config "org.el")
(load-config "tabs.el")
(load-config "treemacs.el")
(load-config "modes.el")
(load-config "claude.el")

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

(defun hym/copy-buffer-file-name-relative ()
  (interactive)
  (if-let* ((file-name (buffer-file-name))
            (proj (project-current))
            (root (project-root proj))
            (relative-name (file-relative-name file-name root)))
      (progn
        (message relative-name)
        (kill-new relative-name))
    (error "Buffer not visiting a file or not in a project")))

(defun hym/delete-current-file ()
  (interactive)
  (if-let ((file-name (buffer-file-name)))
      (when (yes-or-no-p (format "Delete %s? " file-name))
        (delete-file file-name)
        (kill-buffer))
    (error "Buffer not visiting a file")))

(defun hym/search-in-directory ()
  "Prompts for directory and does a search there"
  (interactive)
  (let ((dir (read-directory-name "Dir to search: ")))
    (consult-ripgrep dir)))

(defun hym/chmod-current-file ()
  "Change mode of currently visited file"
  (interactive)
  (chmod (buffer-file-name) (read-file-modes "File modes (octal or symbolic): " (buffer-file-name))))

(hym/leader-def
  ":" 'execute-extended-command
  "," 'consult-buffer
  "<" 'consult-fd
  "SPC" 'project-find-file
  "fs" 'evil-write
  "fy" 'hym/copy-buffer-file-name
  "fY" 'hym/copy-buffer-file-name-relative
  "fd" 'hym/delete-current-file
  "fr" 'rename-visited-file
  "fm" 'hym/chmod-current-file
  "br" 'revert-buffer
  "*"  'hym/grep-for-symbol-at-point
  "tl" 'global-display-line-numbers-mode
  "tw" 'writeroom-mode
  "sd" 'hym/search-in-directory
  "sl" 'consult-line
  "si" 'consult-imenu
  "sI" 'consult-imenu-multi
  "hi" 'info
  "hm" 'man)

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

(define-key minibuffer-mode-map (kbd "C-S-v") 'yank)

(global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Unset default full screen shortcut
(global-unset-key (kbd "<f11>"))

;; Make scripts executable automatically
;; https://www.masteringemacs.org/article/script-files-executable-automatically
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(when (executable-find "hunspell")
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_GB"))

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

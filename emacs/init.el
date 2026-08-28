;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(defun hym/gc-when-unfocused ()
  "Collect garbage once no frame has focus, so the pause is not noticed."
  (unless (seq-some #'frame-focus-state (frame-list))
    (garbage-collect)))

(add-function :after after-focus-change-function #'hym/gc-when-unfocused)

(setq native-comp-async-report-warnings-errors nil
      native-comp-jit-compilation t
      native-compile-prune-cache t
      load-prefer-newer t)

;; Redisplay does bidirectional paragraph analysis on every line and GC
;; compacts font caches; neither earns its cost without right-to-left text.
(setq bidi-inhibit-bpa t
      inhibit-compacting-font-caches t
      jit-lock-defer-time 0.05)

;; Skips scanning each paragraph for its first strong directional character.
(setq-default bidi-paragraph-direction 'left-to-right)

(defvar bootstrap-version)
(setq straight-use-package-by-default 't
      straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1)
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

(when (eq system-type 'darwin)
  ;; See https://blog.dzema.name/2023/02/19/emacs-forge-github-macos-keychain.html
  (setq auth-sources '(macos-keychain-generic macos-keychain-internet)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(advice-add 'c-update-modeline :around #'ignore)

(load "server")
(unless (server-running-p) (server-start))

;; Make sure PATH is set correctly
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(if-let* ((local-bin (expand-file-name "~/.local/bin"))
           (_ (file-directory-p local-bin)))
    (progn
      (add-to-list 'exec-path local-bin)
      (setenv "PATH" (concat local-bin ":" (getenv "PATH")))))

(use-package compat)
(use-package delight)
(use-package emacs
  :delight
  (eldoc-mode)
  (auto-fill-mode)
  (auto-revert-mode)
  (hs-minor-mode))

(use-package general)

(general-create-definer hym/leader-def
  :prefix "SPC"
  :states 'normal
  :keymaps 'override)
(general-create-definer hym/local-leader-def
  :prefix ","
  :states 'normal
  :keymaps 'local)

(defun hym/leader-apply (&rest args)
  "Define leader keys from ARGS, which unlike `hym/leader-def' are evaluated.
`general-def' inspects its arguments before evaluation to find positional
ones, so generated key strings and commands have to go straight to
`general-define-key'."
  (apply #'general-define-key
         :prefix "SPC" :states 'normal :keymaps 'override args))

(load-config "evil.el")
(load-config "theme.el")
(load-config "vertico.el")
(load-config "shell.el")
(load-config "git.el")
(load-config "git-delta.el")
(load-config "window.el")
(load-config "project.el")
(load-config "docs.el")
(load-config "lang.el")
(load-config "completion.el")
(load-config "compile.el")
(load-config "org.el")
(load-config "treemacs.el")
(load-config "modes.el")
(load-config "agents.el")
(load-config "docker.el")
(load-config "packages.el")

(load-config "tabs.el")
(hym-tabs-mode 1)

(load-config "workspaces.el")
(load-config "workspaces-sidebar.el")
(load-config "workspaces-worktree.el")
(load-config "workspaces-run.el")
(load-config "workspaces-git.el")
(load-config "workspaces-notes.el")
(load-config "ghostel-monitor.el")
(hym-workspace-mode 1)
(hym/leader-def
  "oo" 'hym-workspace-sidebar-toggle
  "on" 'hym-workspace-new
  "oa" 'hym-workspace-add-repo
  "ot" 'hym-workspace-run-shell
  "or" 'hym-workspace-run-server
  "oR" 'hym-workspace-run-all-servers
  "oX" 'hym-workspace-restart-running-servers
  "ok" 'hym-workspace-kill-server
  "oc" 'hym-workspace-run-agent
  "oC" 'hym-workspace-run-agent-shell
  "oP" 'hym-workspace-new-from-preset
  "og" 'hym-workspace-git-status
  "od" 'hym-workspace-git-diff
  "oD" 'hym-workspace-git-diff-unstaged-with-untracked
  "ol" 'hym-workspace-git-log
  "oN" 'hym-workspace-notes
  "os" 'hym-workspace-scratch
  "tj" 'hym-workspace-prev
  "tk" 'hym-workspace-next
  "tt" 'hym-workspace-switch
  "0" 'hym/tab-switch-to-default-group)

(apply #'hym/leader-apply
       (mapcan (lambda (n)
                 (list (number-to-string n)
                       (hym-workspace-select-index-command n)))
               (number-sequence 1 9)))

(defun hym/grep-for-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (symbol-name (symbol-at-point))))

(defun hym/copy-buffer-file-name ()
  (interactive)
  (if-let* ((file-name (buffer-file-name)))
      (progn
        (message file-name)
        (kill-new file-name))
    (error "Buffer not visiting a file")))

(defun hym/copy-buffer-file-name-claude ()
  (interactive)
  (if-let* ((file-name (buffer-file-name))
            (proj (project-current))
            (root (project-root proj))
            (relative-name (file-relative-name file-name root))
            (line-num (line-number-at-pos))
            (str (format "@%s line %d" relative-name line-num)))
      (progn
        (message str)
        (kill-new str))
    (error "Buffer not visiting a file or not in a project")))

(defun hym/delete-current-file ()
  (interactive)
  (if-let* ((file-name (buffer-file-name)))
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
  "fY" 'hym/copy-buffer-file-name-claude
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

(general-define-key
 :states 'visual
 :prefix "SPC"
 :keymaps 'override
 ":" 'execute-extended-command)

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-c C-d" . helpful-at-point))
  :config
  (hym/leader-def
    "hf" 'helpful-callable
    "hv" 'helpful-variable
    "hk" 'helpful-key))

(general-define-key
 :keymaps 'minibuffer-mode-map
 "C-S-v" 'yank)

(global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      auto-revert-avoid-polling t
      save-interprogram-paste-before-kill t)

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

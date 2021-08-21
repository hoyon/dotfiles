;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(setq comp-async-report-warning-errors nil)

(defvar bootstrap-version)
(setq straight-use-package-by-default 't)
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

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq hym/font-size "10")
(defun hym/toggle-font-size ()
  "Toggle between small and normal font sizes"
  (interactive)
  (setq hym/font-size
        (if (equal hym/font-size "10") "11" "10"))

  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font (format "%s-%s" "Source Code Pro" hym/font-size) t t)))


(use-package telephone-line
  :config
  (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))

  (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-simple-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode t))

(setq backup-directory-alist `(("" . ,(format "%sbackup" user-emacs-directory)))
      create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(advice-add 'c-update-modeline :around #'ignore)

(load "server")
(unless (server-running-p) (server-start))

(defun load-config (filename)
  "Load config file"
  (load (expand-file-name filename user-emacs-directory)))

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

(hym/local-leader-def
  "tt" (lambda () message "hi"))

(load-config "selectrum.el")
(load-config "shell.el")

(use-package project)

(use-package magit
  :config
  (with-eval-after-load 'project
    (define-key project-prefix-map "m" #'magit-project-status)
    (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))
  (hym/leader-def
    "gg" 'magit-status))

(use-package forge
  :after magit)

(setq smerge-command-prefix "C-c v")

(use-package github-review)

(use-package git-timemachine
  :config
  (hym/leader-def
    "gt" 'git-timemachine)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "p" 'git-timemachine-show-previous-revision
    "n" 'git-timemachine-show-next-revision
    "q" 'git-timemachine-quit
    "g" 'git-timemachine-show-nth-revision
    "t" 'git-timemachine-show-revision-fuzzy
    "b" 'git-timemachine-blame
    "c" 'git-timemachine-show-commit))

(use-package moe-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

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

(hym/leader-def
  ":" 'execute-extended-command
  "," 'consult-buffer
  "SPC" 'affe-find
  "fs" 'evil-write
  "fy" 'hym/copy-buffer-file-name
  "fd" 'hym/delete-current-file
  "br" 'revert-buffer
  "pp" 'project-switch-project
  "pf" 'affe-find
  "p/" 'consult-ripgrep
  "pc" 'project-compile
  "p&" 'project-async-shell-command
  "p!" 'project-shell-command
  "pe" 'project-eshell
  "*" 'hym/grep-for-symbol-at-point
  "tl" 'global-display-line-numbers-mode
  "tf" 'hym/toggle-font-size)

(load-config "lang.el")

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
             (abbrev-mode nil "abbrev"))))

(winner-mode 1)
(hym/leader-def
  "wu" 'winner-undo
  "wr" 'winner-redo)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(if (boundp 'minibuffer-mode-map)
    (define-key minibuffer-mode-map (kbd "C-S-v") 'yank))

(use-package dired+)

;; TODO:
;; - cargo key bindings
;; - smerge hydra?
;; - yes or no p https://www.emacswiki.org/emacs/Yes-No

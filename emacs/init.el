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

(setq frame-title-format "%b - %F"
      column-number-mode t
      scroll-conservatively 10)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq backup-directory-alist `(("" . ,(format "%sbackup" user-emacs-directory)))
      create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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
    :keymaps 'normal)
  (general-create-definer hym/local-leader-def
    :prefix ","
    :states 'normal
    :keymaps 'local))

(hym/local-leader-def
  "tt" (lambda () message "hi"))

(load-config "selectrum.el")

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
    "gt" 'git-timemachine))

(use-package moe-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-one-light t))

(defun hym/grep-for-symbol-at-point ()
  (interactive)
  (consult-ripgrep nil (symbol-name (symbol-at-point))))

(defun hym/format-buffer ()
  (interactive)
  (funcall
   (pcase major-mode
     ('elixir-mode 'elixir-format)
     (_ (lambda () (message "I don't know how to format the current buffer"))))))

(defun hym/copy-buffer-file-name ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(hym/leader-def
  ":" 'execute-extended-command
  "," 'consult-buffer
  "fs" 'evil-write
  "fy" 'hym/copy-buffer-file-name
  "pp" 'project-switch-project
  "pf" 'affe-find
  "SPC" 'affe-find
  "p/" 'consult-ripgrep
  "pc" 'project-compile
  "p&" 'project-async-shell-command
  "p!" 'project-shell-command
  "cf" 'hym/format-buffer
  "*" 'hym/grep-for-symbol-at-point)

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
             (auto-revert-mode nil "autorevert"))))

(winner-mode 1)
(hym/leader-def
  "wu" 'winner-undo
  "wr" 'winner-redo)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(define-key minibuffer-mode-map (kbd "C-S-v") 'yank)

;; TODO:
;; - cargo key bindings
;; - make Y yank to end of line?
;; - smerge hydra?

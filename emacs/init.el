;; -*- lexical-binding: t -*-

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

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

(setq
 sentence-end-double-space nil ;; Don't require double spaces to separate spaces. Affects M-q
 use-short-answers 't ;; yes-or-no-p -> y-or-n-p
 next-error-message-highlight 'keep)

(menu-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)

(save-place-mode 1)

(setq hym/font-size "10")
(defun hym/toggle-font-size ()
  "Toggle between small and normal font sizes"
  (interactive)
  (setq hym/font-size
        (if (equal hym/font-size "10") "11" "10"))

  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font (format "%s-%s" "Source Code Pro" hym/font-size) t t)))

(setq shell-file-name "/bin/bash")

(use-package telephone-line
  :config
  (defun hym/column-of-pos (pos)
    (save-excursion
      (goto-char pos)
      (current-column)))

  ;; Inspired by https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el#L1116
  (telephone-line-defsegment* hym/telephone-line-region-segment ()
    "Segment which shows size of current selection."
    (when (and (or mark-active (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))))
     (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (format "%sL %sC"
              (count-lines beg end)
              (abs (- (hym/column-of-pos end) (hym/column-of-pos beg)))))))

  (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-process-segment
                   telephone-line-minor-mode-segment))
        (nil    . (telephone-line-buffer-segment))))

  (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (nil   . (hym/telephone-line-region-segment))
        (accent . (telephone-line-simple-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode t))

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

(use-package project)
(load-config "selectrum.el")
(load-config "shell.el")
(load-config "git.el")

(use-package moe-theme)
(use-package doom-themes
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-org-config))

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
  "<" 'consult-project-buffer
  "SPC" 'affe-find
  "fs" 'evil-write
  "fy" 'hym/copy-buffer-file-name
  "fd" 'hym/delete-current-file
  "fr" 'hym/rename-current-buffer-file
  "br" 'revert-buffer
  "pp" 'project-switch-project
  "pf" 'affe-find
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
  "sI" 'consult-imenu-multi)

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
    "hk" 'helpful-key)

  (when (version< "28" emacs-version)
    ;; helpful is broken in Emacs 29 without these hacks
    (message "loading loading loading")

    ;; https://github.com/Wilfred/helpful/issues/282
    (defvar read-symbol-positions-list nil)

    ;; Inspired by https://github.com/hlissner/doom-emacs/issues/6097
    (defun helpful-fix-autoloaded-inner (fn sym _)
      (funcall fn sym))

    (defun helpful-fix-autoloaded (fn &rest args)
      (advice-add 'help-fns--autoloaded-p :around #'helpful-fix-autoloaded-inner)
      (apply fn args)
      (advice-remove 'help-fns--autoloaded-p #'helpful-fix-autoloaded-inner)
      )

    (advice-add 'helpful--autoloaded-p :around #'helpful-fix-autoloaded)))

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

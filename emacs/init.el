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

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq backup-directory-alist `(("" . ,(format "%sbackup" user-emacs-directory))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(use-package evil
  :config
  (evil-mode 1))

(use-package magit)

(use-package moe-theme
  :config
  (load-theme 'moe-light))

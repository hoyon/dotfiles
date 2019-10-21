;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Source Code Pro" :size 12)
      doom-big-font (font-spec :family "Source Code Pro" :size 18)
      dired-dwim-target t
      projectile-indexing-method 'hybrid
      company-idle-delay nil)

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

;;; ELIXIR
(map! (:localleader
        (:map elixir-mode-map
            (:prefix ("t" . "test")
                "a" #'alchemist-mix-test
                "b" #'alchemist-mix-test-this-buffer
                "t" #'alchemist-mix-test-at-point
                "r" #'alchemist-mix-rerun-last-test
                "s" #'elixir-test-side-by-side)
            (:prefix ("g" . "goto")
                "t" #'alchemist-project-toggle-file-and-tests
                "T" #'alchemist-project-toggle-file-and-tests-other-window)
            (:prefix ("f" . "format")
                "a" #'mix-format-all
                "f" #'mix-format-current))))

(defun mix-format-all ()
  "Format all staged elixir files in project using .formatter in project root"
  (interactive)
  (projectile-with-default-dir
      (projectile-project-root)
    (shell-command "git diff --name-only HEAD | egrep '\\.ex$|\\.exs' | xargs mix format"))
  (if (fboundp 'magit-refresh-all)
      (magit-refresh-all)
    nil))

(defun mix-format-current ()
  "Format current Elixr file using .formatter in project root"
  (interactive)
  (projectile-with-default-dir
      (projectile-project-root)
    (shell-command (format "mix format %s" buffer-file-name)))
  (if
      (fboundp 'magit-refresh-all)
      (magit-refresh-all)
    nil))

(defun elixir-test-side-by-side ()
  "Split view between current file and its test"
  (interactive)
  (doom/window-maximize-buffer)
  (alchemist-project-toggle-file-and-tests-other-window)
  )

(setq alchemist-test-ask-about-save nil)

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(setq js-indent-level 2)

;; C/C++
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(defun cpp-error-filter (errors)
  "Filters out false positive errors"
  (interactive)
  (seq-filter 'is-dependency-gen-error errors)
  )

(defun is-dependency-gen-error (error)
  "Checks if an error is a dependency gen error"
  (interactive)
  (not (string-match "error opening '.*\\.cpp\\.o\\.d': No such file or directory" (flycheck-error-message error)))
  )

(custom-set-variables '(flycheck-irony-error-filter #'cpp-error-filter))

(defun my-cpp-setup ()
  "My setup for C++"
  (interactive)
  (c-set-offset 'access-label '--)
  (c-set-offset 'topmost-intro '-))

(add-hook 'c++-mode-hook 'my-cpp-setup)

;; dir local variables
(put 'magit-todos-exclude-globs 'safe-local-variable #'listp)

;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Source Code Pro" :size 12)
      doom-big-font (font-spec :family "Source Code Pro" :size 18)
      dired-dwim-target t
      projectile-indexing-method 'hybrid)

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

;;; ELIXIR
(map! (:localleader
        (:map elixir-mode-map
            (:prefix ("t" . "test")
                "a" #'alchemist-mix-test
                "b" #'alchemist-mix-test-this-buffer
                "t" #'alchemist-mix-test-at-point
                "r" #'alchemist-mix-rerun-last-test)
            (:prefix ("g" . "goto")
                "t" #'alchemist-project-toggle-file-and-tests)
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

(setq alchemist-test-ask-about-save nil)

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

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

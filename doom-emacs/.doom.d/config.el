;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Source Code Pro" :size 12))

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

;; popup rules
(set-popup-rule! "^\\*compilation\\*$" :side 'right :size 0.4)
(set-popup-rule! "^\\*alchemist test report\\*$" :side 'right :size 0.4)

;;; ELIXIR
(map! :map elixir-mode-map
      (:localleader
        (:prefix ("t" . "test")
          :desc "Run all tests"                   "a" #'alchemist-mix-test
          :desc "Run tests in this buffer"        "b" #'alchemist-mix-test-this-buffer
          :desc "Run tests at point"              "t" #'alchemist-mix-test-at-point
          :desc "Rerun last run tests"            "r" #'alchemist-mix-rerun-last-test)
        (:prefix ("g" . "goto")
          :desc "Toggle file and test"            "t" #'alchemist-project-toggle-file-and-tests)
        (:prefix ("f" . "format")
          :desc "Format all files"                "a" #'mix-format-all
          :desc "Format current file"             "f" #'mix-format-current)))

(defun mix-format-all ()
  "Format all staged elixir files in project using .formatter in project root"
  (interactive)
  (projectile-with-default-dir
      (projectile-project-root)
    (shell-command "git diff --name-only HEAD | egrep '\.ex$|\.exs' | xargs mix format"))
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

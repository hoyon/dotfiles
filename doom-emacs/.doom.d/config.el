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
          :desc "Toggle file and test"            "t" #'alchemist-project-toggle-file-and-tests)))

(setq alchemist-test-ask-about-save nil)

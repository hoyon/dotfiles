;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(map! :map elixir-mode-map
      (:localleader
        (:prefix ("t" . "test")
          :desc "Run all tests"                   "a" #'alchemist-mix-test
          :desc "Run tests in this buffer"        "b" #'alchemist-mix-test-this-buffer
          :desc "Run tests at point"              "t" #'alchemist-mix-test-at-point)
        (:prefix ("g" . "goto")
          :desc "Toggle file and test"            "t" #'alchemist-project-toggle-file-and-tests)))

(setq alchemist-test-ask-about-save nil)

;; -*- lexical-binding: t -*-

(use-package corfu
  (global-corfu-mode))

(setq eglot-code-action-indicator "!!"
      eglot-code-action-indications '(margin))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode . ("expert" "--stdio"))))

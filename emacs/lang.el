;; -*- lexical-binding: t -*-

(setq-default
 ;; don't use tabs for indenting
 indent-tabs-mode nil)

(defun hym/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'hym/show-trailing-whitespace)
(add-hook 'markdown-mode-hook 'hym/show-trailing-whitespace)

(defun hym/highlight-todos ()
  "Highlight TODO and friends in code"
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'hym/highlight-todos)

(use-package reformatter)

(reformatter-define hym/clang-format
  :program "clang-format"
  :args '()
  :group 'cc-mode)

(defun hym/format-buffer ()
  (interactive)
  (funcall
   (pcase major-mode
     ('elixir-mode 'elixir-format)
     ('rust-mode 'rust-format-buffer)
     ('zig-mode 'zig-format-buffer)
     ('terraform-mode 'terraform-format-buffer)
     ('json-mode 'json-pretty-print-buffer)
     ('web-mode 'elixir-format) ;; TODO: enable only for html.eex and html.heex instead of for all web-mode buffers
     ('c++-mode 'hym/clang-format-buffer)
     ('c-mode 'hym/clang-format-buffer)
     (_ (lambda () (message "I don't know how to format the current buffer"))))))

(hym/leader-def
  "cf" 'hym/format-buffer)

(defun hym/c-mode-hook ()
  (interactive)
  ;; Use C-c C-o to find syntatic symbol for current line
  (c-set-offset 'arglist-intro '+))

(add-hook 'c-mode-hook 'hym/c-mode-hook)
(add-hook 'c++-mode-hook 'hym/c-mode-hook)

(defun hym/elixir-mode-hook ()
  (hym/local-leader-def
    "ta" 'exunit-verify-all
    "tb" 'exunit-verify
    "tt" 'exunit-verify-single
    "tr" 'exunit-rerun
    "td" 'exunit-debug
    "gt" 'exunit-toggle-file-and-test
    "gT" 'exunit-toggle-file-and-test-other-window))

(use-package elixir-mode
  :hook (elixir-mode . hym/elixir-mode-hook))

(use-package exunit)

(use-package erlang)

(use-package elm-mode)

(setq
 js-indent-level 2
 css-indent-offset 2)

(defun hym/web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode ("\\.eex" "\\.mvx" "\\.heex" "\\.tsx" "\\.ts" "\\.svelte" "\\.vue" "\\.astro" "\\.njk")
  :hook (web-mode . hym/web-mode-hook)
  :config
  (setq web-mode-engines-alist
        '(("elixir" . "\\.mvx")
          ("elixir" . "\\.heex"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package rust-mode)
(use-package cargo
  :after rust-mode
  :hook (rust-mode . cargo-minor-mode))

(use-package zig-mode
  :config
  (setq zig-format-on-save nil))

(use-package rego-mode
  :config
  (setq rego-format-at-save nil))

(use-package clojure-mode)
(use-package cider)

(use-package yaml-mode)
(use-package terraform-mode)
(use-package cmake-mode)
(use-package fish-mode)
(use-package json-mode)
(use-package graphviz-dot-mode)
(use-package dockerfile-mode)
(use-package csv-mode)
(use-package nim-mode)
(use-package smalltalk-mode)
(use-package qml-mode)
(use-package glsl-mode)

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

;; Show colours in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook ((c++-mode java-mode zig-mode emacs-lisp-mode) . smartparens-mode))

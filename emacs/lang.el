;; -*- lexical-binding: t -*-

(setq-default
 ;; don't use tabs for indenting
 indent-tabs-mode nil)

(defun hym/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'hym/show-trailing-whitespace)
(add-hook 'markdown-mode-hook 'hym/show-trailing-whitespace)

(use-package reformatter)

(reformatter-define hym/clang-format
  :program "clang-format"
  :args '())

(reformatter-define hym/html-format
  :program "prettier"
  :args '("--parser" "html"))

(reformatter-define hym/go-format
  :program "gofmt"
  :args '())

(defun hym/format-buffer ()
  (interactive)
  (funcall
   (pcase major-mode
     ('elixir-mode 'elixir-format)
     ('rust-mode 'rust-format-buffer)
     ('zig-mode 'zig-format-buffer)
     ('terraform-mode 'terraform-format-buffer)
     ('json-mode 'json-pretty-print-buffer)
     ('json-ts-mode 'json-pretty-print-buffer)
     ('web-mode 'elixir-format) ;; TODO: enable only for html.eex and html.heex instead of for all web-mode buffers
     ('c++-ts-mode 'hym/clang-format-buffer)
     ('c-mode 'hym/clang-format-buffer)
     ('mhtml-mode 'hym/html-format-buffer)
     ('go-ts-mode 'hym/go-format-buffer)
     (_ (lambda () (message "I don't know how to format the current buffer"))))))

(hym/leader-def
  "cf" 'hym/format-buffer)

(defun hym/c-mode-hook ()
  (interactive)
  ;; Use C-c C-o to find syntatic symbol for current line
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'innamespace '0)
  )

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
        web-mode-code-indent-offset 2
        web-mode-enable-auto-quoting nil))

(use-package web-mode
  :mode ("\\.eex" "\\.mvx" "\\.heex" "\\.tsx" "\\.ts" "\\.svelte" "\\.vue" "\\.astro" "\\.njk" "\\.webc")
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

(use-package cmake-mode)
(use-package csv-mode)
(use-package dockerfile-mode)
(use-package fish-mode)
(use-package glsl-mode)
(use-package wgsl-mode)
(use-package graphviz-dot-mode)
(use-package json-mode)
(use-package nim-mode)
(use-package qml-mode)
(use-package smalltalk-mode)
(use-package terraform-mode)
(use-package yaml-mode)

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(setq go-ts-mode-indent-offset 8)

(defun hym/go-mode-hook ()
  (interactive)
  (setq tab-width 4
        go-ts-mode-indent-offset 4))

(add-hook 'go-ts-mode-hook 'hym/go-mode-hook)

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook ((c++-mode java-mode zig-mode emacs-lisp-mode clojure-mode rust-mode go-ts-mode) . smartparens-mode))

(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (progn
      (setq treesit-language-source-alist
            '((bash "https://github.com/tree-sitter/tree-sitter-bash")
              (cmake "https://github.com/uyha/tree-sitter-cmake")
              (css "https://github.com/tree-sitter/tree-sitter-css")
              (c "https://github.com/tree-sitter/tree-sitter-c")
              (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
              (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
              (elisp "https://github.com/Wilfred/tree-sitter-elisp")
              (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
              (go "https://github.com/tree-sitter/tree-sitter-go")
              (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
              (html "https://github.com/tree-sitter/tree-sitter-html")
              (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
              (json "https://github.com/tree-sitter/tree-sitter-json")
              (make "https://github.com/alemuller/tree-sitter-make")
              (markdown "https://github.com/ikatyang/tree-sitter-markdown")
              (python "https://github.com/tree-sitter/tree-sitter-python")
              (rust "https://github.com/tree-sitter/tree-sitter-rust")
              (toml "https://github.com/tree-sitter/tree-sitter-toml")
              (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
              (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
              (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

      ;; Install all grammars if not yet available
      (mapc
       (lambda (lang)
         (if (not (treesit-language-available-p lang))
             (progn
               (treesit-install-language-grammar lang))))
       (mapcar #'car treesit-language-source-alist))

      (setq major-mode-remap-alist
            '((yaml-mode . yaml-ts-mode)
              (bash-mode . bash-ts-mode)
              (js-mode . js-ts-mode)
              (typescript-mode . typescript-ts-mode)
              (json-mode . json-ts-mode)
              (css-mode . css-ts-mode)
              (python-mode . python-ts-mode)
              (cmake-mode . cmake-ts-mode)
              (dockerfile-mode . dockerfile-ts-mode)
              (c++-mode . c++-ts-mode)
              (c-mode . c-ts-mode)
              (rust-mode . rust-ts-mode)))
      ))

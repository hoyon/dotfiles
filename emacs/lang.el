;; -*- lexical-binding: t -*-

(setq-default
 ;; don't use tabs for indenting
 indent-tabs-mode nil)

(defun hym/show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'hym/show-trailing-whitespace)
(add-hook 'markdown-mode-hook 'hym/show-trailing-whitespace)

(add-hook 'prog-mode-hook #'hs-minor-mode)

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
  (let* ((formatters
          '((elixir . elixir-format)
            (rust . rust-format-buffer)
            (zig . zig-format-buffer)
            (terraform . terraform-format-buffer)
            (json . json-pretty-print-buffer)
            (web . elixir-format) ;; TODO: enable only for html.eex and html.heex instead of for all web-mode buffers
            (c++ . hym/clang-format-buffer)
            (c . hym/clang-format-buffer)
            (mhtml . hym/html-format-buffer)
            (go . hym/go-format-buffer)))
         (base-mode (intern (replace-regexp-in-string "-ts-mode$\\|-mode$" "" (symbol-name major-mode))))
         (formatter (alist-get base-mode formatters)))
    (funcall (or formatter
                 (lambda () (message "I don't know how to format the current buffer"))))))

(hym/leader-def
  "cf" 'hym/format-buffer)

(defun hym/c-mode-hook ()
  "C/C++ mode configuration."
  ;; Use C-c C-o to find syntatic symbol for current line
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'innamespace '0))

(add-hook 'c-mode-hook 'hym/c-mode-hook)
(add-hook 'c++-mode-hook 'hym/c-mode-hook)

(defun hym/elixir-mode-hook ()
  ;; Fix using finding &my_fun/1 when pressing *
  (modify-syntax-entry ?& ".")

  (hym/local-leader-def
    "ta" 'exunit-verify-all
    "tb" 'exunit-verify
    "tt" 'exunit-verify-single
    "tr" 'exunit-rerun
    "td" 'exunit-debug
    "gt" 'exunit-toggle-file-and-test
    "gT" 'exunit-toggle-file-and-test-other-window))

(use-package exunit)
(use-package erlang)

(add-hook 'elixir-ts-mode-hook 'hym/elixir-mode-hook)

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
  :mode ("\\.eex" "\\.heex" "\\.svelte" "\\.vue" "\\.astro" "\\.njk" "\\.webc")
  :hook (web-mode . hym/web-mode-hook)
  :config
  (setq web-mode-engines-alist
        '(("elixir" . "\\.heex"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode
  (("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode)
   ("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "multimarkdown")
  :custom
  (markdown-gfm-use-electric-backquote nil))

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

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package clojure-mode)
(use-package cider)

(use-package cmake-mode)
(use-package csv-mode)
(use-package dockerfile-mode)
(use-package fish-mode)
(use-package wgsl-mode)
(use-package graphviz-dot-mode)
(use-package nim-mode)
(use-package qml-mode)
(use-package smalltalk-mode)
(use-package just-mode)

(use-package terraform-mode
  :custom (terraform-command "tofu"))

(use-package bazel
  :mode ("Tiltfile" . bazel-mode))

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(use-package go-mode)

(setq go-ts-mode-indent-offset 8)

(defun hym/go-mode-hook ()
  "Go mode configuration."
  (setq tab-width 4
        go-ts-mode-indent-offset 4))

(add-hook 'go-ts-mode-hook 'hym/go-mode-hook)

(use-package smartparens
  :delight (smartparens-mode)
  :config
  (require 'smartparens-config)
  :hook ((c++-mode java-mode zig-mode emacs-lisp-mode clojure-mode rust-mode go-ts-mode) . smartparens-mode))

(mapc (lambda (entry) (add-to-list 'auto-mode-alist entry))
        '(("\\.ts\\'" . typescript-ts-mode)
          ("\\.tsx\\'" . tsx-ts-mode)
          ("\\.json\\'" . json-ts-mode)
          ("\\.yaml\\'" . yaml-ts-mode)
          ("\\.yml\\'" . yaml-ts-mode)
          ("\\.glsl\\'" . glsl-ts-mode)))

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
              (heex "https://github.com/phoenixframework/tree-sitter-heex")
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
              (glsl "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
              (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

      ;; Install all grammars if not yet available
      (pcase-dolist (`(,lang . ,_) treesit-language-source-alist)
        (unless (treesit-language-available-p lang)
          (treesit-install-language-grammar lang)))

      (setq major-mode-remap-alist
            '((bash-mode . bash-ts-mode)
              (js-mode . js-ts-mode)
              (json-mode . json-ts-mode)
              (css-mode . css-ts-mode)
              (python-mode . python-ts-mode)
              (cmake-mode . cmake-ts-mode)
              (dockerfile-mode . dockerfile-ts-mode)
              (c++-mode . c++-ts-mode)
              (c-mode . c-ts-mode)
              (rust-mode . rust-ts-mode)
              (go-mode . go-ts-mode)
              (go-dot-mod-mode . go-mod-ts-mode)
              (elixir-mode . elixir-ts-mode)
              ))
      ))

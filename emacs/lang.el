;; -*- lexical-binding: t -*-

(setq-default indent-tabs-mode nil) ;; only use spaces when indenting

(defun hym/highlight-todos ()
  "Highlight TODO and friends in code"
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'hym/highlight-todos)

(defun hym/format-buffer ()
  (interactive)
  (funcall
   (pcase major-mode
     ('elixir-mode 'elixir-format)
     ('rust-mode 'rust-format-buffer)
     ('zig-mode 'zig-format-buffer)
     (_ (lambda () (message "I don't know how to format the current buffer"))))))

(hym/leader-def
  "cf" 'hym/format-buffer)

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

(setq js-indent-level 2)

(defun hym/web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode ("\\.eex" "\\.mvx" "\\.heex" "\\.tsx" "\\.ts" "\\.svelte")
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

(use-package yaml-mode)
(use-package terraform-mode)
(use-package cmake-mode)
(use-package zig-mode)
(use-package fish-mode)

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

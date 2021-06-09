(setq-default indent-tabs-mode nil) ;; only use spaces when indenting

(defun hym/highlight-todos ()
  "Highlight TODO and friends in code"
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
(add-hook 'prog-mode-hook 'hym/highlight-todos)

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

(use-package elm-mode)

(setq js-indent-level 2)

(defun hym/web-mode-hook ()
  "Hooks for web mode"
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package web-mode
  :mode ("\\.eex" "\\.mvx" "\\.tsx" "\\.ts")
  :hook (web-mode . hym/web-mode-hook)
  :config
  (setq web-mode-engines-alist
        '(("elixir" . "\\.mvx"))))

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

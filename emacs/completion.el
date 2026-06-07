;; -*- lexical-binding: t -*-

;; Completion - explicit trigger only
(use-package corfu
  :init
  (setq corfu-auto nil
        corfu-preselect 'first)
  :config
  (global-corfu-mode)
  (evil-define-key 'insert 'global (kbd "C-;") 'completion-at-point))

(defun hym/dismiss-docs ()
  (interactive)
  (dolist (name '("*helpful*" "*eldoc*" "*Eglot documentation*"))
    (when-let* ((buf (get-buffer name))
                (win (get-buffer-window buf t)))
      (quit-window nil win))))

(defun hym/show-docs ()
  "Show fixed docs for the symbol at point.
Use an Eglot hover snapshot when LSP is live, otherwise fall back to Helpful."
  (interactive)
  (if (bound-and-true-p eglot--managed-mode)
      (let* ((server (eglot--current-server-or-lose))
             (hover (jsonrpc-request
                     server
                     :textDocument/hover
                     (eglot--TextDocumentPositionParams)))
             (contents (plist-get hover :contents)))
        (unless (and contents (not (seq-empty-p contents)))
          (user-error "No documentation at point"))
        (let ((docs (eglot--hover-info contents (plist-get hover :range))))
          (with-current-buffer (get-buffer-create "*Eglot documentation*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert docs)
              (goto-char (point-min))
              (special-mode)
              (evil-local-set-key 'normal "q" #'quit-window))
            (pop-to-buffer (current-buffer)))))
    (helpful-at-point)))

;; Keep eldoc/eglot docs clipped to a single echo-area line
(setq eldoc-echo-area-use-multiline-p nil)

;; Eglot - auto-enable for elixir and go
(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full))
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode . ("dexter" "lsp"))))

(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

(add-hook 'elixir-ts-mode-hook #'eglot-ensure)
(add-hook 'go-ts-mode-hook #'eglot-ensure)
(add-hook 'rust-mode-hook #'eglot-ensure)

;; Bindings
(hym/leader-def
  "cj" 'xref-find-definitions
  "cb" 'xref-go-back
  "cr" 'xref-find-references
  "cR" 'eglot-rename
  "ca" 'eglot-code-actions
  "cn" 'flymake-goto-next-error
  "cp" 'flymake-goto-prev-error
  "cd" 'hym/show-docs
  "cq" 'hym/dismiss-docs
  "ce" 'eglot
  "ci" 'eglot-inlay-hints-mode)

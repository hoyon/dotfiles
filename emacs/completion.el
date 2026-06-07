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

(defun hym/show-full-diagnostic ()
  "Show full Flymake diagnostic(s) at point in a separate buffer."
  (interactive)
  (let ((diags (hym/flymake-diagnostics-at-point)))
    (unless diags
      (user-error "No Flymake diagnostic at point"))
    (let ((entries
           (mapcar
            (lambda (diag)
              (let* ((src (or (flymake-diagnostic-buffer diag) (current-buffer)))
                     (beg (flymake-diagnostic-beg diag))
                     (pos (if (markerp beg) (marker-position beg) beg)))
                (with-current-buffer src
                  (list :type (upcase (symbol-name (flymake-diagnostic-type diag)))
                        :line (line-number-at-pos pos)
                        :col (save-excursion
                               (goto-char pos)
                               (1+ (current-column)))
                        :text (flymake-diagnostic-text diag)))))
            diags)))
      (with-current-buffer (get-buffer-create "*Flymake diagnostic*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (entry entries)
            (insert (format "%s at %s:%s\n\n%s\n\n"
                            (plist-get entry :type)
                            (plist-get entry :line)
                            (plist-get entry :col)
                            (plist-get entry :text))))
          (goto-char (point-min))
          (special-mode)
          (evil-local-set-key 'normal "q" 'quit-window))
        (pop-to-buffer (current-buffer))))))

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
  "cD" 'hym/show-full-diagnostic
  "cq" 'hym/dismiss-docs
  "ce" 'eglot
  "ci" 'eglot-inlay-hints-mode)

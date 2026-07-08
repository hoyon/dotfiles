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

(with-eval-after-load 'eldoc
  (with-eval-after-load 'evil
    (eldoc-add-command-completions
     "evil-backward-" "evil-beginning-of-" "evil-end-of-" "evil-first-"
     "evil-forward-" "evil-goto-" "evil-last-" "evil-next-" "evil-previous-")))

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

(defun hym/eglot-code-actions-buffer ()
  "Offer Eglot fixes for every diagnostic, previewing their locations."
  (interactive)
  (require 'consult)
  (let* ((server (eglot--current-server-or-lose))
         (diagnostics
          (cl-remove-if-not
           (lambda (diag)
             (cdr (assoc 'eglot-lsp-diag (eglot--diag-data diag))))
           (flymake-diagnostics (point-min) (point-max))))
         (candidates
          (cl-loop
           for diag in diagnostics
           append
           (cl-loop
            for action in (eglot-code-actions
                           (flymake-diagnostic-beg diag)
                           (flymake-diagnostic-end diag)
                           "quickfix")
            collect (cons action diag)))))
    (unless candidates
      (user-error "No Eglot quick fixes in this buffer"))
    (let* (choices
           (display-candidates
            (cl-loop
             for (action . diag) in candidates
             for location =
             (list (copy-marker (flymake-diagnostic-beg diag))
                   (cons 0 (- (flymake-diagnostic-end diag)
                              (flymake-diagnostic-beg diag))))
             for line =
             (line-number-at-pos (flymake-diagnostic-beg diag) t)
             do (push (cons location action) choices)
             collect
             (propertize (format "L%-4d %s" line (plist-get action :title))
                         'consult--candidate location)))
           (selected
            (consult--read
             display-candidates
             :prompt "Eglot quick fix: "
             :require-match t
             :sort nil
             :lookup #'consult--lookup-candidate
             :state (consult--jump-state))))
      (eglot-execute server (cdr (assoc selected choices))))))

;; Bindings
(hym/leader-def
  "cj" 'xref-find-definitions
  "cb" 'xref-go-back
  "cr" 'xref-find-references
  "cR" 'eglot-rename
  "ca" 'eglot-code-actions
  "cA" 'hym/eglot-code-actions-buffer
  "cn" 'flymake-goto-next-error
  "cp" 'flymake-goto-prev-error
  "cd" 'hym/show-docs
  "cq" 'hym/dismiss-docs
  "ce" 'eglot
  "ci" 'eglot-inlay-hints-mode)

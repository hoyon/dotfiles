;; -*- lexical-binding: t -*-

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  (setq claude-code-ide-terminal-backend 'eat)
  (hym/leader-def
    "Cc" 'claude-code-ide
    "CC" 'claude-code-ide-continue
    "Cr" 'claude-code-ide-resume
    "Ct" 'claude-code-ide-toggle
    "CT" 'claude-code-ide-toggle-recent
    "Cb" 'claude-code-ide-switch-to-buffer
    "Cp" 'claude-code-ide-send-prompt
    "Cm" 'claude-code-ide-menu
    "Cl" 'claude-code-ide-list-sessions
    "Cq" 'claude-code-ide-stop))

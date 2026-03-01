;; -*- lexical-binding: t -*-

(use-package agent-shell
  :config

  (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))

  (general-define-key
   :states 'normal
   :keymaps 'agent-shell-mode-map
   "RET" #'comint-send-input
   "C-<return>" #'comint-send-input)

  (general-define-key
   :states 'insert
   :keymaps 'agent-shell-mode-map
   "RET" #'newline
   "C-<return>" #'comint-send-input)

  ;; diff mode
  (general-define-key
   :states 'normal
   :keymaps 'agent-shell-diff-mode-map
   "o" 'agent-shell-diff-open-file
   "y" 'agent-shell-diff-accept-all
   "r" 'agent-shell-diff-reject-all
   "n" 'diff-hunk-next
   "p" 'diff-hunk-prev
   "q" 'kill-this-buffer))

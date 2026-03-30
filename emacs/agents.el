;; -*- lexical-binding: t -*-

(defun hym/agent-shell--latest-permission-keymap ()
  "Find the keymap from the latest permission button in the buffer."
  (save-excursion
    (goto-char (point-max))
    (when-let* ((found (text-property-search-backward
                        'agent-shell-permission-button t t))
                (pos (prop-match-beginning found)))
      (get-text-property pos 'keymap))))

(defun hym/agent-shell--invoke-permission-key (key fallback-msg)
  "Invoke KEY from the latest permission keymap, or show FALLBACK-MSG."
  (if-let* ((keymap (hym/agent-shell--latest-permission-keymap))
            (action (lookup-key keymap (kbd key))))
      (funcall action)
    (message "%s" fallback-msg)))

(defun hym/agent-shell-allow ()
  "Allow the latest permission request."
  (interactive)
  (hym/agent-shell--invoke-permission-key "y" "No pending permission request"))

(defun hym/agent-shell-allow-always ()
  "Always allow the latest permission request."
  (interactive)
  (hym/agent-shell--invoke-permission-key "!" "No pending permission request"))

(defun hym/agent-shell-reject ()
  "Reject the latest permission request."
  (interactive)
  (agent-shell-interrupt t))

(defun hym/agent-shell-view-diff ()
  "View diff for the latest permission request."
  (interactive)
  (hym/agent-shell--invoke-permission-key "v" "No pending permission diff"))

(defun hym/agent-shell-restart ()
  "Kill the current agent shell and start a fresh one."
  (interactive)
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent shell buffer"))
  (let ((window (selected-window)))
    (kill-buffer (current-buffer))
    (agent-shell-new-shell)
    (when (window-live-p window)
      (select-window window))))

(use-package agent-shell
  :config

  (setq
   agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config)
   agent-shell-session-strategy 'new

   agent-shell-anthropic-claude-environment (agent-shell-make-environment-variables
                                             "CLAUDE_CODE_EXECUTABLE" (executable-find "claude"))
   )

  (general-define-key
   :states 'normal
   :keymaps 'agent-shell-mode-map
   "RET" #'comint-send-input
   "C-<return>" #'comint-send-input)

  (hym/local-leader-def
   :keymaps 'agent-shell-mode-map
   "a" #'hym/agent-shell-allow
   "A" #'hym/agent-shell-allow-always
   "r" #'hym/agent-shell-reject
   "v" #'hym/agent-shell-view-diff
   "j" #'agent-shell-jump-to-latest-permission-button-row
   "R" #'hym/agent-shell-restart)

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

(hym/leader-def
 "a" '(:ignore t :which-key "agent")
 "a a" #'agent-shell-toggle
 "a n" #'agent-shell-new-shell
 "a c" #'agent-shell-prompt-compose)

(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix "SPC"
 "a d" #'agent-shell-send-dwim
 "a r" #'agent-shell-send-region
 "a f" #'agent-shell-send-file
 "a F" #'agent-shell-send-other-file
 "a s" #'agent-shell-send-screenshot)

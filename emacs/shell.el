;; -*- lexical-binding: t -*-

;; eshell config
(setq history-delete-duplicates t
      eshell-hist-ignoredups t
      eshell-history-size 1024)

(defun hym/eshell-clear ()
  (interactive)
  (eshell/clear-scrollback) (eshell-send-input))

(defun hym/eshell-kill-before-point ()
  (interactive)
  (kill-line 0))

(defun hym/eshell-c-d ()
  "Send EOF if subprocess, else kill eshell"
  (interactive)
  (if (get-buffer-process (buffer-name))
      (eshell-send-eof-to-process)
      (eshell-life-is-too-much)))

(general-define-key
 :states 'insert
 :keymaps 'eshell-mode-map
 "C-a" 'eshell-bol
 "C-e" 'eshell-show-maximum-output
 "C-l" 'hym/eshell-clear
 "C-u" 'eshell-kill-input
 "C-p" 'eshell-previous-input
 "C-n" 'eshell-next-input
 "C-d" 'hym/eshell-c-d)

(defun eshell/d ()
  "Open dired in current directory"
  (dired default-directory))

(defun eshell/ff (&rest args)
  "Open file"
  (pcase (length args)
    (0 (error "file expected"))
    (1 (find-file (car args)))
    (_ (error "too many args"))))

(defun hym/eshell-setup ()
  (setenv "TERM" "xterm-256color"))

(add-hook 'eshell-mode-hook #'hym/eshell-setup)

;; vterm config
(use-package vterm
  :init
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-shell (executable-find "fish"))

  (defun hym/vterm-send-key (key)
    "Return a command that sends KEY to vterm."
    (lambda () (interactive) (vterm-send key)))

  (general-define-key
   :states 'insert
   :keymaps 'vterm-mode-map
   "C-a" (hym/vterm-send-key "C-a")
   "C-e" (hym/vterm-send-key "C-e")
   "C-l" (hym/vterm-send-key "C-l")
   "C-u" (hym/vterm-send-key "C-u")
   "C-p" (hym/vterm-send-key "C-p")
   "C-n" (hym/vterm-send-key "C-n")
   "C-d" (hym/vterm-send-key "C-d")
   "C-c" (hym/vterm-send-key "C-c")
   "C-r" (hym/vterm-send-key "C-r"))

  (general-define-key
   :states 'normal
   :keymaps 'vterm-mode-map
   "p" 'vterm-yank))

(defun hym/ghostel-disable-nobreak-highlighting ()
  "Display terminal non-breaking spaces like ordinary spaces."
  (setq-local nobreak-char-display nil))

(use-package ghostel
  :hook (ghostel-mode . hym/ghostel-disable-nobreak-highlighting)
  :config
  (setq ghostel-shell (executable-find "fish")
        ;; Server tabs can be very chatty; keep useful history without making
        ;; every Ghostel buffer unreasonably expensive.
        ghostel-max-scrollback (* 25 1024 1024)))

(defun hym/evil-ghostel-toggle-escape ()
  "Toggle ESC between the terminal and Evil in this Ghostel buffer.
From the default `auto' setting, the first toggle selects the terminal."
  (interactive)
  (unless (and (derived-mode-p 'ghostel-mode)
               (bound-and-true-p evil-ghostel-mode))
    (user-error "This is not an Evil Ghostel buffer"))
  (evil-ghostel-toggle-send-escape
   (if (eq evil-ghostel--escape-mode 'terminal) 3 2))
  (force-mode-line-update))

(defun hym/ghostel-send-escape ()
  "Send one ESC keypress to the Ghostel terminal."
  (interactive)
  (ghostel-send-key "escape"))

(use-package evil-ghostel
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode)
  :config
  ;; Evil normally forwards insert-state C-y to the subprocess.  Route paste
  ;; gestures through Ghostel so multiline prompts use bracketed paste.
  (evil-define-key 'insert evil-ghostel-mode-map
    (kbd "C-y") #'ghostel-yank
    (kbd "s-v") #'ghostel-yank
    (kbd "C-c C-e") #'hym/ghostel-send-escape)
  (define-key ghostel-mode-map (kbd "C-c C-e")
              #'hym/ghostel-send-escape)
  (define-key ghostel-mode-map (kbd "C-c C-g")
              #'hym/evil-ghostel-toggle-escape))

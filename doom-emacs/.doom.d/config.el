;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Source Code Pro" :size 15)
      doom-big-font (font-spec :family "Source Code Pro" :size 18)
      doom-localleader-key ","
      dired-dwim-target t
      projectile-indexing-method 'hybrid
      projectile-enable-caching nil
      company-idle-delay nil
      lsp-ui-flycheck-live-reporting nil
      lsp-enable-symbol-highlighting nil)

(server-start)

;; Use bar cursor when in insert mode in terminal
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (evil-terminal-cursor-changer-activate))

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

;; Disable evil snipe (restore standard vim behaviour for s and S)
(after! evil-snipe
  (evil-snipe-mode -1))

(menu-bar-mode -1)

;;; ELIXIR
(map! (:localleader
        (:map elixir-mode-map
            (:prefix ("t" . "test")
                "a" #'mix-test
                "b" #'mix-test-current-buffer
                "t" #'mix-test-current-test
                "r" #'mix-last-command
                "s" #'elixir-test-side-by-side)
            (:prefix ("g" . "goto")
                "t" #'alchemist-project-toggle-file-and-tests
                "T" #'alchemist-project-toggle-file-and-tests-other-window)
            (:prefix ("f" . "format")
                "a" #'mix-format-all
                "f" #'mix-format-current))))

(defun mix-format-all ()
  "Format all staged elixir files in project using .formatter in project root"
  (projectile-with-default-dir
      (projectile-project-root)
    (shell-command "git diff --name-only HEAD | egrep '\\.ex$|\\.exs' | xargs mix format"))
  (if (fboundp 'magit-refresh-all)
      (magit-refresh-all)
    nil))

(defun mix-format-current ()
  "Format current Elixr file using .formatter in project root"
  (projectile-with-default-dir
      (projectile-project-root)
    (shell-command (format "mix format %s" buffer-file-name)))
  (if
      (fboundp 'magit-refresh-all)
      (magit-refresh-all)
    nil))

(defun elixir-test-side-by-side ()
  "Split view between current file and its test"
  (interactive)
  (doom/window-maximize-buffer)
  (alchemist-project-toggle-file-and-tests-other-window))

(setq alchemist-test-ask-about-save nil)

(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2)

(setq js-indent-level 2)

;; Elm
(add-hook 'elm-mode-hook 'elm-format-on-save-mode)

;; C/C++
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(defun cpp-error-filter (errors)
  "Filters out false positive errors"
  (seq-filter 'is-dependency-gen-error-p errors))

(defun is-dependency-gen-error-p (error)
  "Checks if an error is a dependency gen error"
  (not (string-match "error opening '.*\\.cpp\\.o\\.d': No such file or directory" (flycheck-error-message error))))

(custom-set-variables '(flycheck-irony-error-filter #'cpp-error-filter))

;; dir local variables
(put 'magit-todos-exclude-globs 'safe-local-variable #'listp)
(put 'projectile-project-test-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-run-cmd 'safe-local-variable #'stringp)
(put 'projectile-project-compilation-cmd 'safe-local-variable #'stringp)

(defun touch-file ()
  "Force modification of current file, unless already modified."
  (interactive)
  (if (and (verify-visited-file-modtime (current-buffer))
           (not (buffer-modified-p)))
      (progn
        (set-buffer-modified-p t)
        (save-buffer 0))))

(defun zeal-search ()
  "Search in zeal docs"
  (interactive)
  (let ((query (zeal-get-query)))
    (call-process "setsid" nil nil nil "zeal" query)
    (call-process "swaymsg" nil nil nil "[title=\"- Zeal$\"] focus")))

(defun zeal-get-query ()
  "Get the search query"
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string "Search Zeal docs: ")))

(map! :leader
      (:prefix-map ("l" . "user")
        :desc "Search in zeal" "z" #'zeal-search))

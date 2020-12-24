;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "Source Code Pro" :size 15)
      doom-big-font (font-spec :family "Source Code Pro" :size 18)
      doom-localleader-key ","
      dired-dwim-target t
      company-idle-delay nil
      display-line-numbers-type nil
      lsp-ui-flycheck-live-reporting nil
      lsp-enable-symbol-highlighting nil
      lsp-signature-auto-activate nil
      doom-themes-enable-bold nil
      doom-modeline-buffer-encoding nil
      doom-modeline-vcs-max-length 12
      doom-modeline-env-version nil
      undo-limit 80000000
      evil-want-fine-undo t)

(after! projectile
  (setq projectile-indexing-method 'alien
        projectile-files-cache-expire 5))

(setq-default uniquify-buffer-name-style 'forward)

(load "server")
(unless (server-running-p) (server-start))

(global-visual-line-mode) ;; Always wrap long lines
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)) ;; Show arrows on wrapped lines

(add-to-list 'face-ignored-fonts "Noto Color Emoji")

;; Format on save
(add-hook 'elm-mode-hook '+format-enable-on-save-h)
(add-hook 'go-mode-hook '+format-enable-on-save-h)
(setq rustic-format-trigger 'on-save)

;; Elixir
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
                "T" #'alchemist-project-toggle-file-and-tests-other-window))))

(defun elixir-test-side-by-side ()
  "Split view between current file and its test"
  (interactive)
  (doom/window-maximize-buffer)
  (alchemist-project-toggle-file-and-tests-other-window))

(setq alchemist-test-ask-about-save nil)

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      js-indent-level 2)

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

;; Rust
(setq rustic-flycheck-clippy-params "--message-format=json")

;; Zig
(setq zig-format-on-save nil)

(defun zig-test-current-buffer ()
  "Run zig test on the current buffer"
  (interactive)
  (zig--run-cmd "test" (buffer-file-name)))

(map! (:localleader
        (:map zig-mode-map
            ("t" #'zig-test-current-buffer))))

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

(defun load-light-theme ()
  "Switch to the doom-one-light theme"
  (interactive)
  (load-theme 'doom-one-light t)
  (doom/reload-theme))

(defun load-dark-theme ()
  "Switch to the doom-one theme"
  (interactive)
  (load-theme 'doom-one t)
  (doom/reload-theme))

(map! :leader
      (:prefix-map ("l" . "user")
        :desc "Search in zeal" "z" #'zeal-search
        :desc "Use light theme" "l" #'load-light-theme
        :desc "Use dark theme" "d" #'load-dark-theme
        :desc "Touch current file" "t" #'touch-file))

;;; ron-mode.el --- A major emacs mode for editing RON files

;; Version: 0.1
;; Author: Ho-Yon Mak
;; Url: https://github.com/hoyon/ron-mode
;; Keywords: languages
;; Package-Requires: ((emacs "24.0"))

;; This file is distributed under the terms of the MIT license

;;; Commentary:

;; This package provides a major mode for editing RON files, based on
;; `rust-mode'

;;; Code:

(require 'rust-mode)

(setq ron-font-lock-defaults
      '(
        ("#\\!?\\[[^]]*\\]" . font-lock-preprocessor-face)
        ("[A-Z][A-Za-z0-9_]*" . font-lock-type-face)
        ("\\b[A-Za-z0-9_]*:" . font-lock-variable-name-face)
        ))

(setq ron-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
       (modify-syntax-entry ?# ">" synTable)
       synTable
       )
      )

(define-derived-mode ron-mode rust-mode "RON"
  "major mode for editing RON files."
  (setq-local font-lock-defaults '(ron-font-lock-defaults))
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local rust-indent-offset 2)
  )

(add-to-list 'auto-mode-alist '("\\.ron\\'" . ron-mode))

(provide 'ron-mode)

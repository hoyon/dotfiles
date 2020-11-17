;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! geiser)
(package! nim-mode)
(package! meson-mode)
(package! mix :recipe (:host github :repo "ayrat555/mix.el"))

(package! rtags :disable t)

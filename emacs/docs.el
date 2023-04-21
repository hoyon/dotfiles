;; -*- lexical-binding: t -*-

(defun hym/sdl2-docs-find-file ()
  (interactive)
  (ido-find-file-in-dir "/usr/include/SDL2/"))

(hym/leader-def
  "ds" 'hym/sdl2-docs-find-file)

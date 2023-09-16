;; -*- lexical-binding: t -*-

(defun hym/cpm-docs-find-file ()
  "Find files in CMake package manager deps"
  (interactive)
  (consult-fd "~/.cache/CPM"))

(hym/leader-def
  "dc" 'hym/cpm-docs-find-file)

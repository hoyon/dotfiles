;; -*- lexical-binding: t -*-

(use-package gptel
  :config
  (require 'gptel-integrations)

  (setq
   gptel-model 'gpt-4.1
   gptel-backend (gptel-make-gh-copilot "Copilot"))

  ;; :custom (gptel-log-level 'debug)
)


(use-package mcp
  :straight (:host github :repo "lizqwerscott/mcp.el"
                   :branch "master")
  :config
  :after gptel
  :custom
  (mcp-hub-servers
   '(("filesystem" . (:command "/opt/homebrew/bin/npx"
                               :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/hoyon/code/sandbox")))
     ("steampipe" . (:command "/opt/homebrew/bin/npx"
                              :args ("-y" "@turbot/steampipe-mcp")))))
  :config (require 'mcp-hub)
  ;; :hook (after-init . mcp-hub-start-all-server)
  )

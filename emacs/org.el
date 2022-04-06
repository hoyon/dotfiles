(use-package org)

(use-package org-modern
  :hook (org-mode-hook . org-modern-mode)
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

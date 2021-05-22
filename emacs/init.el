(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

 ;; put all backups in a single directory to avoid making a mess everywhere
(setq backup-directory-alist `(("" . ,(format "%sbackup" user-emacs-directory))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(moe-light))
 '(custom-safe-themes
   '("27a1dd6378f3782a593cc83e108a35c2b93e5ecc3bd9057313e1d88462701fcd" "0feb7052df6cfc1733c1087d3876c26c66410e5f1337b039be44cb406b6187c6" default))
 '(fido-mode t)
 '(package-selected-packages '(moe-theme))
 '(recentf-mode t)
 '(scroll-bar-mode nil)
 '(warning-suppress-types '((comp)))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

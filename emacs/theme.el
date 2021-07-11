;; -*- lexical-binding: t -*-

(setq frame-title-format "%b - %F"
      column-number-mode t
      scroll-conservatively 10)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)

(setq hym/font-size "10")
(defun hym/toggle-font-size ()
  "Toggle between small and normal font sizes"
  (interactive)
  (setq hym/font-size
        (if (equal hym/font-size "10") "11" "10"))

  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font (format "%s-%s" "Source Code Pro" hym/font-size) t t)))

(use-package telephone-line
  :config
  (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
        (nil    . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))

  (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (accent . (telephone-line-simple-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode t))

(hym/leader-def
  "tf" 'hym/toggle-font-size)

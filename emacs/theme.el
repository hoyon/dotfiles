;; -*- lexical-binding: t -*-

(setq column-number-mode t
      scroll-conservatively 10)

;; disabling the menu bar on MacOS breaks mission control and has no effect anyway
;; https://github.com/railwaycat/homebrew-emacsmacport/issues/124
(if (not (eq system-type 'darwin))
    (menu-bar-mode -1))

(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)

(setq show-paren-context-when-offscreen 'child-frame)

(use-package moe-theme)
(use-package doom-themes)
(use-package ef-themes)

(use-package modus-themes
  :init
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil))

;; ef-spring on MacOS, ef-day everywhere else
(if (not (eq system-type 'darwin))
    (load-theme 'ef-elea-light t)
  (load-theme 'ef-spring t))

(setq
 hym/font-family "Berkeley Mono"
 ;; hym/font-family "Source Code Pro"
 hym/font-size "12"
 )

(add-to-list 'default-frame-alist `(font . ,(format "%s-%s" hym/font-family hym/font-size)))
(set-frame-font (format "%s-%s" hym/font-family hym/font-size) t t)

(hym/leader-def
  "af" 'global-text-scale-adjust)

(use-package telephone-line
  :config
  (defun hym/column-of-pos (pos)
    (save-excursion
      (goto-char pos)
      (current-column)))

  ;; Inspired by https://github.com/seagle0128/doom-modeline/blob/master/doom-modeline-segments.el#L1116
  (telephone-line-defsegment* hym/telephone-line-region-segment ()
    "Segment which shows size of current selection."
    (when (and (or mark-active (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))))
     (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (format "%sL %sC"
              (count-lines beg end)
              (abs (- (hym/column-of-pos end) (hym/column-of-pos beg)))))))

  (setq telephone-line-lhs
      '((evil   . (telephone-line-evil-tag-segment))
        (accent . (telephone-line-process-segment
                   telephone-line-minor-mode-segment))
        (nil    . (telephone-line-buffer-segment))))

  (setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment))
        (nil   . (hym/telephone-line-region-segment))
        (accent . (telephone-line-simple-major-mode-segment))
        (evil   . (telephone-line-airline-position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (telephone-line-mode t))

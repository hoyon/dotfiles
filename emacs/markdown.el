;; -*- lexical-binding: t -*-

(defvar hym/markdown-width 135)
(defvar hym/markdown-heading-spacing 0.6)
(defvar hym/markdown-list-item-spacing 0.4)
(defvar hym/markdown-code-block-padding 0.6)

(defun hym/markdown-wrap ()
  (setq-local visual-fill-column-width hym/markdown-width)
  (setq-local adaptive-fill-regexp "[ \t]*\\(?:>[ \t]*\\)*")
  (visual-line-mode)
  (visual-wrap-prefix-mode)
  (visual-fill-column-mode))

(defun hym/markdown-toggle-view ()
  (interactive)
  (if (derived-mode-p 'markdown-ts-view-mode)
      (progn
        (markdown-ts-mode)
        (setq buffer-read-only nil))
    (markdown-ts-view-mode)))

(defun hym/markdown--table-cells (row)
  "Trimmed text of each cell in ROW."
  (let* ((pipes (seq-filter (lambda (node) (equal (treesit-node-type node) "|"))
                            (treesit-node-children row)))
         (bounds (append (list (treesit-node-start row))
                         (mapcan (lambda (pipe)
                                   (list (treesit-node-start pipe) (treesit-node-end pipe)))
                                 pipes)
                         (list (treesit-node-end row))))
         cells)
    (while bounds
      (push (string-trim (buffer-substring-no-properties (pop bounds) (pop bounds))) cells))
    (setq cells (nreverse cells))
    (when (and pipes (= (treesit-node-start (car pipes)) (treesit-node-start row)))
      (pop cells))
    (when (and pipes (= (treesit-node-end (car (last pipes))) (treesit-node-end row)))
      (setq cells (butlast cells)))
    cells))

(defconst hym/markdown--inline-regexp
  (rx (or (seq "`" (group-n 1 (+ (not "`"))) "`")
          (seq "**" (group-n 2 (+? anychar)) "**")
          (seq "~~" (group-n 3 (+? anychar)) "~~")
          (seq "*" (group-n 4 (+ (not "*"))) "*")
          (seq "[" (group-n 5 (+ (not "]"))) "](" (* (not ")")) ")")
          (seq "\\" (group-n 6 punct)))))

(defun hym/markdown--format-inline (text)
  "Render the inline markup in TEXT as faces, dropping the delimiters."
  (let ((pos 0)
        parts)
    (while (string-match hym/markdown--inline-regexp text pos)
      (let ((plain (substring text pos (match-beginning 0)))
            (end (match-end 0))
            (styled (cond ((match-beginning 1)
                           (propertize (match-string 1 text) 'face 'markdown-ts-code-span))
                          ((match-beginning 2) (cons (match-string 2 text) 'markdown-ts-bold))
                          ((match-beginning 3) (cons (match-string 3 text) 'markdown-ts-strikethrough))
                          ((match-beginning 4) (cons (match-string 4 text) 'markdown-ts-emphasis))
                          ((match-beginning 5) (cons (match-string 5 text) 'markdown-ts-link))
                          (t (match-string 6 text)))))
        (when (consp styled)
          (let ((inner (hym/markdown--format-inline (car styled))))
            (add-face-text-property 0 (length inner) (cdr styled) nil inner)
            (setq styled inner)))
        (push plain parts)
        (push styled parts)
        (setq pos end)))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

(defun hym/markdown--fit-widths (natural available)
  "Shrink NATURAL column widths to fit AVAILABLE, squeezing the widest first."
  (let ((widths (make-vector (length natural) 0))
        (remaining available)
        (columns-left (length natural)))
    (dolist (i (sort (number-sequence 0 (1- (length natural)))
                     (lambda (a b) (< (nth a natural) (nth b natural)))))
      (let ((width (min (nth i natural) (max 5 (/ remaining columns-left)))))
        (aset widths i width)
        (setq remaining (- remaining width)
              columns-left (1- columns-left))))
    widths))

(defun hym/markdown--wrap-cell (text width)
  (let ((line "")
        lines)
    (dolist (word (split-string text "[ \t]+" t))
      (let ((candidate (if (string-empty-p line) word (concat line " " word))))
        (if (<= (string-width candidate) width)
            (setq line candidate)
          (unless (string-empty-p line)
            (push line lines))
          (while (> (string-width word) width)
            (push (substring word 0 width) lines)
            (setq word (substring word width)))
          (setq line word))))
    (nreverse (cons line lines))))

(defun hym/markdown--table-row (cells widths aligns face)
  (let* ((wrapped (seq-map-indexed
                   (lambda (width i)
                     (let ((text (hym/markdown--format-inline (or (nth i cells) ""))))
                       (add-face-text-property 0 (length text) face t text)
                       (hym/markdown--wrap-cell text width)))
                   widths))
         (height (apply #'max (mapcar #'length wrapped)))
         (separator (propertize " │ " 'face 'shadow)))
    (mapconcat
     (lambda (line-number)
       (mapconcat
        (lambda (i)
          (let* ((text (or (nth line-number (nth i wrapped)) ""))
                 (pad (- (aref widths i) (string-width text)))
                 (left (pcase (nth i aligns)
                         ('right pad)
                         ('center (/ pad 2))
                         (_ 0))))
            (concat (make-string left ?\s) text (make-string (- pad left) ?\s))))
        (number-sequence 0 (1- (length widths)))
        separator))
     (number-sequence 0 (1- height))
     "\n")))

(defun hym/markdown--view-overlay (beg end &rest properties)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'hym-markdown-view t)
    (while properties
      (overlay-put ov (pop properties) (pop properties)))))

(defun hym/markdown--space-below (node amount)
  "Add AMOUNT of `line-spacing' below the last line of NODE."
  (let ((eol (save-excursion
               (goto-char (treesit-node-end node))
               (skip-chars-backward "\n")
               (pos-eol))))
    (when (< eol (point-max))
      (hym/markdown--view-overlay eol (1+ eol) 'line-spacing amount))))

(defun hym/markdown-clear-view ()
  (remove-overlays (point-min) (point-max) 'hym-markdown-view t))

(defun hym/markdown--fence-as-padding (fence face)
  "Show FENCE's hidden line as a short blank line in FACE.
Padding drawn with overlay strings has no buffer position, which
leaves the cursor stuck when moving up into a code block.  The
`invisible' override must be non-nil: overlays cannot mask a text
property with nil."
  (let ((bol (save-excursion (goto-char (treesit-node-start fence)) (pos-bol)))
        (eol (save-excursion (goto-char (treesit-node-start fence)) (pos-eol))))
    (hym/markdown--view-overlay bol eol 'invisible 'hym-markdown-shown 'display "")
    (when (< eol (point-max))
      (hym/markdown--view-overlay eol (1+ eol) 'invisible 'hym-markdown-shown 'face face))))

(defun hym/markdown--pad-code-blocks ()
  (dolist (block (treesit-query-capture 'markdown '((fenced_code_block) @block) nil nil t))
    (let ((fences (seq-filter (lambda (node)
                                (equal (treesit-node-type node) "fenced_code_block_delimiter"))
                              (treesit-node-children block))))
      (when (= (length fences) 2)
        (let* ((content (save-excursion
                          (goto-char (treesit-node-end (car fences)))
                          (forward-line 1)
                          (point)))
               (face `(:inherit ,(or (get-char-property content 'face) 'markdown-ts-code-block)
                       :height ,hym/markdown-code-block-padding
                       :extend t)))
          (dolist (fence fences)
            (hym/markdown--fence-as-padding fence face)))))))

(defun hym/markdown-view-setup ()
  ;; Markup is hidden by fontification, so deferring it flashes raw
  ;; markdown while scrolling.  A nil timer is how jit-lock itself
  ;; forces immediate fontification.
  (setq-local jit-lock-defer-time nil
              jit-lock-defer-timer nil)
  (hym/markdown-render-view)
  (add-hook 'after-revert-hook #'hym/markdown-render-view nil t))

(defun hym/markdown-render-view ()
  (hym/markdown-clear-view)
  (hym/markdown--render-tables)
  (hym/markdown--pad-code-blocks)
  (dolist (heading (treesit-query-capture 'markdown '([(atx_heading) (setext_heading)] @heading)
                                          nil nil t))
    (hym/markdown--space-below heading hym/markdown-heading-spacing))
  (dolist (item (treesit-query-capture 'markdown '((list_item) @item) nil nil t))
    (hym/markdown--space-below item hym/markdown-list-item-spacing)))

(defun hym/markdown--render-tables ()
  "Draw pipe tables as wrapped, aligned grids with overlays, leaving the buffer text alone."
  (dolist (table (treesit-query-capture 'markdown '((pipe_table) @table) nil nil t))
    (let* ((rows (mapcar (lambda (row) (cons row (hym/markdown--table-cells row)))
                         (treesit-node-children table)))
           (delimiter-p (lambda (row)
                          (equal (treesit-node-type (car row)) "pipe_table_delimiter_row")))
           (aligns (mapcar (lambda (spec)
                             (cond ((string-match-p "\\`:.*:\\'" spec) 'center)
                                   ((string-suffix-p ":" spec) 'right)
                                   (t 'left)))
                           (cdr (seq-find delimiter-p rows))))
           (content-rows (seq-remove delimiter-p rows))
           (natural (mapcar (lambda (i)
                              (apply #'max 1
                                     (mapcar (lambda (row)
                                               (string-width
                                                (hym/markdown--format-inline (or (nth i (cdr row)) ""))))
                                             content-rows)))
                            (number-sequence
                             0 (1- (apply #'max (mapcar (lambda (row) (length (cdr row))) rows))))))
           (widths (hym/markdown--fit-widths
                    natural (- hym/markdown-width (* 3 (1- (length natural)))))))
      (let ((divider (propertize (mapconcat (lambda (width) (make-string width ?─)) widths "─┼─")
                                 'face 'shadow))
            (last-row (car (last rows))))
        (dolist (row rows)
          (let* ((node (car row))
                 (display (pcase (treesit-node-type node)
                            ("pipe_table_delimiter_row" (copy-sequence divider))
                            ("pipe_table_header"
                             (hym/markdown--table-row (cdr row) widths aligns
                                                      '(bold markdown-ts-table-header)))
                            (_
                             (concat (hym/markdown--table-row (cdr row) widths aligns
                                                              'markdown-ts-table-cell)
                                     (unless (eq row last-row)
                                       (concat "\n" divider)))))))
            (add-text-properties 0 (length display) '(line-prefix "" wrap-prefix "") display)
            (hym/markdown--view-overlay (treesit-node-start node) (treesit-node-end node)
                                        'display display)))))))

(use-package markdown-ts-mode
  :straight nil
  :ensure nil
  :mode (("\\.md\\'" . markdown-ts-view-mode)
         ("\\.mdx\\'" . markdown-ts-view-mode)
         ("\\.markdown\\'" . markdown-ts-view-mode))
  :hook (((markdown-ts-mode markdown-ts-view-mode) . hym/markdown-wrap)
         (markdown-ts-view-mode . hym/markdown-view-setup)
         (markdown-ts-mode . hym/markdown-clear-view)
         (markdown-ts-mode . hym/show-trailing-whitespace))
  :config
  (require 'markdown-ts-mode-x)
  (set-face-attribute 'markdown-ts-heading-1 nil :height 1.4)
  (set-face-attribute 'markdown-ts-heading-2 nil :height 1.25)
  (set-face-attribute 'markdown-ts-heading-3 nil :height 1.1)
  (hym/local-leader-def
    :keymaps '(markdown-ts-mode-map markdown-ts-view-mode-map)
    "v" #'hym/markdown-toggle-view)
  (hym/local-leader-def
    :keymaps 'markdown-ts-mode-map
    "t" #'markdown-ts-table-align-table))

(defun djoyner/smart-home ()
  "Move point to first non-whitespace character or beginning-of-line.
Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(defun djoyner/smart-end ()
  "Odd end to end of line, even end to begin of text/code."
  (interactive)
  (if (eq (point) (- (line-end-position) 1))
      (end-of-line-text)
    (end-of-line)))

(defun djoyner/is-this-line-empty ()
  "Returns t if the current line is empty. Otherwise nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun djoyner/copy-to-end-of-line ()
  (interactive)
  (copy-region-as-kill (point) (point-at-eol)))

(after 'evil
  (defun djoyner/electric-append-with-indent (count &optional vcount)
    "Indent the current line if it is empty. Otherwise, just do a normal append-line."
    (interactive "p")
    (if (and (= (point) (line-beginning-position))
             (djoyner/is-this-line-empty))
        (indent-according-to-mode))
    (evil-append-line count vcount))

  (defun djoyner/evil-paste-clipboard-before ()
    (interactive)
    (evil-paste-before 1 ?\"))

  (defun djoyner/evil-paste-clipboard-after ()
    (interactive)
    (evil-paste-after 1 ?\"))

  (defun djoyner/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun djoyner/evil-set-tab-width (value)
    (interactive "ntab-width: ")
    (set-variable 'tab-width value))
  )

(after 'ido
  (defun djoyner/ido-jump-to-home ()
    "Jump to the user's home directory in ido."
    (interactive)
    (ido-set-current-directory "~/")
    (setq ido-exit 'refresh)
    (exit-minibuffer))
  )

(provide 'djoyner-funcs)

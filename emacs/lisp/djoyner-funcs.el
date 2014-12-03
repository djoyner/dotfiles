(require 'misc-funcs)

(defun djoyner/retina-display-p ()
  "Returns t if the current display is a retina display."
  (condition-case nil
      (= (call-process "~/bin/is-retina-display") 0)
    (error nil)))

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

(after 'dired
  (defun djoyner/dired-open ()
    (interactive)
    (if running-mac-osx
        (progn
          (let ((file-name (dired-get-file-for-visit)))
            (if (file-exists-p file-name)
                (call-process "/usr/bin/open" nil 0 nil file-name))))
      (dired-open)))

  (defun djoyner/dired-up-directory ()
    "Take dired up one directory, but behave like dired-find-alternate-file"
    (interactive)
    (let ((old (current-buffer)))
      (dired-up-directory)
      (kill-buffer old)))

  (defun djoyner/dired-next-line (count)
    "Move to next line, always staying on the dired filename."
    (interactive "p")
    (dired-next-line count)
    (dired-move-to-filename))

  (defun djoyner/dired-previous-line (count)
    "Move to previous line, always staying on the dired filename."
    (interactive "p")
    (dired-previous-line count)
    (dired-move-to-filename)))

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
    (evil-visual-restore)))

(after 'ido
  (defun djoyner/ido-smart-jump-to-root ()
    "Jump to the root directory in ido IFF there is no text entered."
    (interactive)
    (when (and ido-text (= 0 (length ido-text)))
      (ido-set-current-directory "/")
      (setq ido-exit 'refresh))
    (exit-minibuffer))

  (defun djoyner/ido-jump-to-home ()
    "Jump to the user's home directory in ido."
    (interactive)
    (ido-set-current-directory "~/")
    (setq ido-exit 'refresh)
    (exit-minibuffer)))

(provide 'djoyner-funcs)

(require 'dired)

;; Re-enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;; Recursive deletes allowed, after asking for each directory at top level
(setq dired-recursive-deletes 'top)

;; Copy recursively without asking
(setq dired-recursive-copies 'always)

;; Assume regular 'ls'
(if running-gnu-linux
    (setq dired-listing-switches "-aBhl --group-directories-first --dired")
  (setq dired-listing-switches "-aBhl"
        dired-use-ls-dired nil))

;; Make dired play nice with evil
(after 'evil
  (evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
  (evil-define-key 'normal dired-mode-map "h" 'my-dired-up-directory)
  (evil-define-key 'normal dired-mode-map "j" 'my-dired-next-line)
  (evil-define-key 'normal dired-mode-map "k" 'my-dired-previous-line)
  (evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
  (evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
  (evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
  (evil-define-key 'normal dired-mode-map "o" 'my-dired-open)
  (evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)
  (evil-define-key 'normal dired-mode-map "/" 'evil-search-forward))

;; Functions
(defun my-dired-open ()
  (interactive)
  (if running-mac-osx
      (progn
        (let ((file-name (dired-get-file-for-visit)))
          (if (file-exists-p file-name)
              (call-process "/usr/bin/open" nil 0 nil file-name))))
    (dired-open)))

(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file"
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)))

(defun my-dired-next-line (count)
  "Move to next line, always staying on the dired filename."
  (interactive "p")
  (dired-next-line count)
  (dired-move-to-filename))

(defun my-dired-previous-line (count)
  "Move to previous line, always staying on the dired filename."
  (interactive "p")
  (dired-previous-line count)
  (dired-move-to-filename))

(provide 'dired-config)

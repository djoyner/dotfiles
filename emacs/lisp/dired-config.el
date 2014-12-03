(require 'dired)
(require 'djoyner-funcs)

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

;; Make dired play nicely with evil
(define-key dired-mode-map "c" 'dired-create-directory)
(define-key dired-mode-map "h" 'djoyner/dired-up-directory)
(define-key dired-mode-map "j" 'djoyner/dired-next-line)
(define-key dired-mode-map "J" 'dired-goto-file)
(define-key dired-mode-map "k" 'djoyner/dired-previous-line)
(define-key dired-mode-map "K" 'dired-do-kill-lines)
(define-key dired-mode-map "l" 'dired-find-alternate-file)
(define-key dired-mode-map "n" 'evil-search-next)
(define-key dired-mode-map "N" 'evil-search-previous)
(define-key dired-mode-map "o" 'djoyner/dired-open)
(define-key dired-mode-map "q" 'kill-this-buffer)
(define-key dired-mode-map "/" 'evil-search-forward)
(define-key dired-mode-map (kbd "C-b") 'scroll-down-command)
(define-key dired-mode-map (kbd "C-f") 'scroll-up-command)

(provide 'dired-config)

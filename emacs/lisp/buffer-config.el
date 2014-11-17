(require 'ibuffer)
(require 'uniquify)

;; Make buffer names unique using file path info
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)

;; Set up ibuffer
(setq ibuffer-saved-filter-groups '(("default"
				     ("c" (or (mode . c-mode)
                                              (mode . c++-mode)))
				     ("dired" (mode . dired-mode))
				     ("elisp" (mode . emacs-lisp-mode))
				     ("haskell" (or (mode . haskell-mode)
						    (mode . haskell-cabal-mode)))
				     ("python" (mode . python-mode))
				     ("xml" (mode . nxml-mode))
				     ("*magit*" (name . "^\\*magit"))
				     ("*man*" (name . "^\\*Man "))
				     ("*emacs*" (name . "\\*.*\\*"))))
      ibuffer-formats '((mark modified read-only " "
                              (name 40 40) " "
                              (size 6 -1 :right) " "
                              (mode 16 16 :left) " "
                              filename)
			(mark " " (name 16 -1) " "
                              filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&"
      ibuffer-show-empty-filter-groups nil)

(defun my-ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (ibuffer-auto-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

;; Make ibuffer play nicely with evil
(after 'evil
  (evil-set-initial-state 'ibuffer-mode 'normal)

  (evil-define-key 'normal ibuffer-mode-map
    [backtab] 'ibuffer-toggle-filter-group
    [tab] 'ibuffer-toggle-filter-group
    (kbd "0") 'digit-argument
    (kbd "1") 'digit-argument
    (kbd "2") 'digit-argument
    (kbd "3") 'digit-argument
    (kbd "4") 'digit-argument
    (kbd "5") 'digit-argument
    (kbd "6") 'digit-argument
    (kbd "7") 'digit-argument
    (kbd "8") 'digit-argument
    (kbd "9") 'digit-argument

    ;; Immediate operations
    (kbd "`") 'ibuffer-switch-format
    (kbd "-") 'ibuffer-add-to-tmp-hide
    (kbd "+") 'ibuffer-add-to-tmp-show
    (kbd ",") 'ibuffer-toggle-sorting-mode
    (kbd ".") 'ibuffer-mark-old-buffers
    (kbd "?") 'describe-mode
    (kbd "=") 'ibuffer-diff-with-file
    (kbd "|") 'ibuffer-do-shell-command-pipe
    (kbd "!") 'ibuffer-do-shell-command-file
    (kbd "~") 'ibuffer-do-toggle-modified
    (kbd "b") 'ibuffer-bury-buffer
    (kbd "d") 'ibuffer-mark-for-delete
    (kbd "C-d") 'ibuffer-mark-for-delete-backwards
    (kbd "e") 'ibuffer-visit-buffer
    (kbd "f") 'ibuffer-visit-buffer
    (kbd "g") 'ibuffer-update
    (kbd "h") 'describe-mode
    (kbd "j") 'evil-next-line
    (kbd "k") 'evil-previous-line
    (kbd "C-k") 'ibuffer-kill-line
    (kbd "J") 'ibuffer-jump-to-buffer
    (kbd "l") 'ibuffer-visit-buffer
    (kbd "m") 'ibuffer-mark-forward
    (kbd "n") 'ibuffer-forward-filter-group
    (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect
    (kbd "M-o") 'ibuffer-visit-buffer-1-window
    (kbd "p") 'ibuffer-backward-filter-group
    (kbd "q") 'ibuffer-quit
    (kbd "s i") 'ibuffer-invert-sorting
    (kbd "s a") 'ibuffer-do-sort-by-alphabetic
    (kbd "s v") 'ibuffer-do-sort-by-recency
    (kbd "s s") 'ibuffer-do-sort-by-size
    (kbd "s f") 'ibuffer-do-sort-by-filename/process
    (kbd "s m") 'ibuffer-do-sort-by-major-mode
    (kbd "C-t") 'ibuffer-visit-tags-table
    (kbd "u") 'ibuffer-unmark-forward
    (kbd "v") 'ibuffer-toggle-marks
    (kbd "w") 'ibuffer-copy-filename-as-kill
    (kbd "x") 'ibuffer-do-kill-on-deletion-marks
    (kbd "C-x C-f") 'ibuffer-find-file
    (kbd "C-x v") 'ibuffer-do-view-horizontally
    (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window
    (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)
    (kbd "C-y") 'ibuffer-yank
    (kbd "* *") 'ibuffer-unmark-all
    (kbd "* M") 'ibuffer-mark-by-mode
    (kbd "* m") 'ibuffer-mark-modified-buffers
    (kbd "* u") 'ibuffer-mark-unsaved-buffers
    (kbd "* s") 'ibuffer-mark-special-buffers
    (kbd "* r") 'ibuffer-mark-read-only-buffers
    (kbd "* /") 'ibuffer-mark-dired-buffers
    (kbd "* e") 'ibuffer-mark-dissociated-buffers
    (kbd "* h") 'ibuffer-mark-help-buffers
    (kbd "* z") 'ibuffer-mark-compressed-file-buffers
    (kbd "/ m") 'ibuffer-filter-by-used-mode
    (kbd "/ M") 'ibuffer-filter-by-derived-mode
    (kbd "/ n") 'ibuffer-filter-by-name
    (kbd "/ c") 'ibuffer-filter-by-content
    (kbd "/ e") 'ibuffer-filter-by-predicate
    (kbd "/ f") 'ibuffer-filter-by-filename
    (kbd "/ >") 'ibuffer-filter-by-size-gt
    (kbd "/ <") 'ibuffer-filter-by-size-lt
    (kbd "/ r") 'ibuffer-switch-to-saved-filters
    (kbd "/ a") 'ibuffer-add-saved-filters
    (kbd "/ x") 'ibuffer-delete-saved-filters
    (kbd "/ d") 'ibuffer-decompose-filter
    (kbd "/ s") 'ibuffer-save-filters
    (kbd "/ p") 'ibuffer-pop-filter
    (kbd "/ !") 'ibuffer-negate-filter
    (kbd "/ t") 'ibuffer-exchange-filters
    (kbd "/ TAB") 'ibuffer-exchange-filters
    (kbd "/ \\") 'ibuffer-clear-filter-groups
    (kbd "/ o") 'ibuffer-or-filter
    (kbd "/ g") 'ibuffer-filters-to-filter-group
    (kbd "/ P") 'ibuffer-pop-filter-group
    (kbd "/ D") 'ibuffer-decompose-filter-group
    (kbd "/ R") 'ibuffer-switch-to-saved-filter-groups
    (kbd "/ S") 'ibuffer-save-filter-groups
    (kbd "/ X") 'ibuffer-delete-saved-filter-groups
    (kbd "/ /") 'ibuffer-filter-disable
    (kbd "SPC") 'forward-line
    (kbd "DEL") 'ibuffer-unmark-backward
    (kbd "M-DEL") 'ibuffer-unmark-all
    (kbd "M-}") 'ibuffer-forward-next-marked
    (kbd "M-{") 'ibuffer-backwards-next-marked
    (kbd "M-g") 'ibuffer-jump-to-buffer
    (kbd "M-s a C-s") 'ibuffer-do-isearch
    (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
    (kbd "M-s a C-o") 'ibuffer-do-occur
    (kbd "M-n") 'ibuffer-forward-filter-group
    (kbd "TAB") 'ibuffer-forward-filter-group
    (kbd "M-p") 'ibuffer-backward-filter-group
    (kbd "M-j") 'ibuffer-jump-to-filter-group
    (kbd "% n") 'ibuffer-mark-by-name-regexp
    (kbd "% m") 'ibuffer-mark-by-mode-regexp
    (kbd "% f") 'ibuffer-mark-by-file-name-regexp

    ;; Marked operations
    (kbd "A") 'ibuffer-do-view
    (kbd "D") 'ibuffer-do-delete
    (kbd "E") 'ibuffer-do-eval
    (kbd "F") 'ibuffer-do-shell-command-file
    (kbd "I") 'ibuffer-do-query-replace-regexp
    (kbd "H") 'ibuffer-do-view-other-frame
    (kbd "P") 'ibuffer-do-shell-command-pipe-replace
    (kbd "M") 'ibuffer-do-toggle-modified
    (kbd "O") 'ibuffer-do-occur
    ;; (kbd "P") 'ibuffer-do-print
    (kbd "Q") 'ibuffer-do-query-replace
    (kbd "R") 'ibuffer-do-rename-uniquely
    (kbd "S") 'ibuffer-do-save
    (kbd "T") 'ibuffer-do-toggle-read-only
    (kbd "U") 'ibuffer-do-replace-regexp
    (kbd "V") 'ibuffer-do-revert
    (kbd "W") 'ibuffer-do-view-and-eval
    (kbd "X") 'ibuffer-do-shell-command-pipe)

;; Default to ibuffer
(defalias 'list-buffers 'ibuffer)

(provide 'buffer-config)

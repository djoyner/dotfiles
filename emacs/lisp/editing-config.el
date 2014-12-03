;; Make org-mode the default major mode
(setq default-major-mode 'org-mode)

;; I want to be able to see the mark region and type over the selection
(transient-mark-mode t)
(delete-selection-mode t)

;; Set up tramp
(setq tramp-default-method "ssh")

;; Set up gist
(autoload 'gist-region "gist"
  "Post the current region as a new paste at gist.github.com.
Copies the URL into the kill ring.
\(fn BEGIN END &optional PRIVATE)" t nil)

(autoload 'gist-buffer "gist"
  "Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.
\(fn &optional PRIVATE)" t nil)

;; Other editing-related settings
(setq next-line-add-newlines nil
      find-file-use-truenames nil
      find-file-compare-truenames t
      make-backup-files nil)

;; Avoid hard tabs
(setq-default indent-tabs-mode nil)

(provide 'editing-config)

;;; ~/emacs/lisp/misc-config.el

;; Re-enable various useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Make text-mode the default major mode
(setq default-major-mode 'text-mode)

;; I want to be able to see the mark region and type over the selection
(transient-mark-mode t)
(delete-selection-mode t)

;; Show matching parens
(show-paren-mode t)

;; Set up ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-formats '((mark modified read-only " " (name 40 40) " " (size 6 -1 :right) " " (mode 16 16 :center) " " (process 8 -1) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

;; Set up iswitchb
(iswitchb-mode 1)
(setq iswitchb-default-method 'maybe-frame)

;; Set up tramp
(setq tramp-default-method "ssh")

;; Set up pastie
(autoload 'pastie-region "pastie" "Post the current region as a new paste at pastie.org. Copies the URL into the kill ring." t)

;; Other miscellaneous stuff
(setq make-backup-files nil
      next-line-add-newlines nil
      find-file-use-truenames nil
      find-file-compare-truenames t
      minibuffer-confirm-incomplete t
      win32-alt-is-meta nil)

;;; end ~/emacs/lisp/misc-config.el

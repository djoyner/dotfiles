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

;; Define inferior shells
(require 'defshell)

(setq defshell-reuse-buffer nil
      defshell-rename-buffer-uniquely t)

;; Translate ansi color sequences into text properties (for shells)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Set up tags
(require 'etags)
(defun build-tags (dir-name)
     "Build tags file."
     (interactive "DDirectory: ")
     (eshell-command 
      (format "find %s -type f -name \\*.\\* | ctags --langmap=c++:+.tcc -e -L -" dir-name)))

;; Set Perforce environment variables in the current environment
(if (not (getenv "P4CONFIG"))
    (setenv "P4CONFIG" ".p4env"))

(if (not (getenv "P4USER"))
    (setenv "P4USER" "DJoyner"))

;; Other miscellaneous stuff
(setq compile-command "scons -ku"
      make-backup-files nil
      next-line-add-newlines nil
      find-file-use-truenames nil
      find-file-compare-truenames t
      minibuffer-confirm-incomplete t
      ;ps-print-landscape t
      win32-alt-is-meta nil)

;;; end ~/emacs/lisp/misc-config.el

;;; ~/emacs/lisp/ruby-config.el

(require 'ruby-mode)
(require 'ruby-electric)

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(defun my-ruby-mode-hook ()
  (setq standard-indent 2)
  (ruby-electric-mode t)
  (inf-ruby-keys)
  (define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer))

(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby" "Run an inferior ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")

;;; end ~/emacs/lisp/ruby-config.el

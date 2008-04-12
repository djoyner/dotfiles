;;; ~/emacs/lisp/my-pymode.el

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
				   interpreter-mode-alist))

(autoload 'python-mode "python-mode" "Python editing mode" t)

;;; end ~/emacs/lisp/my-pymode.el

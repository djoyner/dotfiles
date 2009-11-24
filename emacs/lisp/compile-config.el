;;; ~/emacs/lisp/compile-config.el

(add-to-list 'auto-mode-alist '("\\SConstruct'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript'" . python-mode))

(setq compile-command "scons -ku")
(add-to-list `same-window-buffer-names "*compilation*")

;;; end ~/emacs/lisp/compile-config.el

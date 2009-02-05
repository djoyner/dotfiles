;;; ~/emacs/lisp/scons-config.el

(add-to-list 'auto-mode-alist '("\\SConstruct'" . python-mode))
(add-to-list 'auto-mode-alist '("\\SConscript'" . python-mode))

(setq compile-command "scons -ku")

;;; end ~/emacs/lisp/scons-config.el

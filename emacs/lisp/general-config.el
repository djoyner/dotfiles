;;; ~/emacs/lisp/general-config.el

;; Re-enable various useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Miscellaneous settings
(setq debug-on-error t
      win32-alt-is-meta nil)

;;; end ~/emacs/lisp/general-config.el

;;; ~/emacs/lisp/my-p4mode.el

(when (not (eq system-type 'darwin))
  (progn
    (require 'p4)
    (setq p4-use-p4config-exclusively t)))

;;; end ~/emacs/lisp/my-p4mode.el


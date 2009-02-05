;;; ~/emacs/lisp/p4-config.el

(when (not (eq system-type 'darwin))
  (progn
    (require 'p4)
    (setq p4-use-p4config-exclusively t)))

;; Set Perforce environment variables in the current environment
(if (not (getenv "P4CONFIG"))
    (setenv "P4CONFIG" ".p4env"))

(if (not (getenv "P4USER"))
    (setenv "P4USER" "DJoyner"))

;;; end ~/emacs/lisp/p4-config.el


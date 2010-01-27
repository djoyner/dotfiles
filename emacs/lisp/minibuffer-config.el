;;; ~/emacs/lisp/minibuffer-config.el

;; Dim the ignored part of the file name
(file-name-shadow-mode 1)

;; Minibuffer completion incremental feedback
(icomplete-mode 1)

;; Other minibuffer-related settings
(setq completion-ignore-case t
      minibuffer-confirm-incomplete t
      read-file-name-completion-ignore-case t
      resize-mini-windows t)

;;; end ~/emacs/lisp/minibuffer-config.el

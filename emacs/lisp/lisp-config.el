;;; ~/emacs/lisp/lisp-config.el

;; Show the function arglist or the variable docstring in the echo area
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Interaction mode for Emacs Lisp
(autoload 'ielm "ielm" "Start an inferior Emacs Lisp session" t)

;;; end ~/emacs/lisp/lisp-config.el

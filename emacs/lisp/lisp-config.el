(require 'elisp-slime-nav)

;; Turn on elisp-slime-nav-mode and function/variable docstrings in the minibuffer
(defun my-lisp-mode-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode)
  )

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)

(provide 'lisp-config)

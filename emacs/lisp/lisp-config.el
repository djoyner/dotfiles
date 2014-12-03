(require 'elisp-slime-nav)

;; Turn on elisp-slime-nav-mode and function/variable docstrings in the minibuffer
(defun my-lisp-mode-hook ()
  (elisp-slime-nav-mode)
  (turn-on-eldoc-mode))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-mode-hook)

;; Eval-related
(define-prefix-command 'eval-commands-map)
(define-key global-map (kbd "C-x C-e") 'eval-commands-map)
(define-key eval-commands-map (kbd "C-b") 'eval-buffer)
(define-key eval-commands-map (kbd "C-e") 'eval-last-sexp)
(define-key eval-commands-map (kbd "C-p") 'pp-eval-last-sexp)
(define-key eval-commands-map (kbd "C-r") 'eval-region)
(define-key eval-commands-map (kbd "R") 'eval-and-replace)

(provide 'lisp-config)

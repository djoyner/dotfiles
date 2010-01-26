;;; ~/emacs/lisp/shell-config.el

;; Define inferior shells
(require 'defshell)

(setq defshell-reuse-buffer nil
      defshell-rename-buffer-uniquely t)

(defshell "/bin/bash" "bash")

;; Translate ansi color sequences into text properties
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Smarter up/down bindings for shells -- scroll through command history if at command line, otherwise scroll through buffer
(defun shell-maybe-up (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (previous-line arg)
    ))

(defun shell-maybe-down (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (next-line arg)
    ))

(define-key shell-mode-map [up] 'shell-maybe-up)
(define-key shell-mode-map [down] 'shell-maybe-down)

;;; end ~/emacs/lisp/shell-config.el

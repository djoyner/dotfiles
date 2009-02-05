;;; ~/emacs/lisp/shell-config.el

(require 'defshell)

;; Define inferior shells
(setq defshell-reuse-buffer nil
      defshell-rename-buffer-uniquely t)

;; Translate ansi color sequences into text properties (for shells)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(define-key shell-mode-map [up] 'comint-previous-input)
(define-key shell-mode-map [down] 'comint-next-input)

;;; end ~/emacs/lisp/shell-config.el

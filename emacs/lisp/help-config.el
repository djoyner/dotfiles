;; Avoid the description of all minor modes
(defun describe-major-mode ()
  "Describe only `major-mode'."
  (interactive)
  (describe-function major-mode))

(define-key help-map "m" 'describe-major-mode)

;; Other help-related settings
(setq apropos-do-all t
      Man-notify-method 'newframe)

(provide 'help-config)

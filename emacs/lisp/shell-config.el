(require 'eshell)

;; shell setup
(defun my-shell-mode-hook ()
  (define-key shell-mode-map (kbd "M-k") 'comint-previous-input)
  (define-key shell-mode-map (kbd "M-j") 'comint-next-input))

(add-hook 'shell-mode-hook 'my-shell-mode-hook)

;; eshell setup
(defun my-eshell-mode-hook ()
  (define-key eshell-mode-map (kbd "M-k") 'eshell-previous-input)
  (define-key eshell-mode-map (kbd "M-j") 'eshell-next-input))

(add-hook 'eshell-mode-hook 'my-eshell-mode-hook)

(provide 'shell-config)

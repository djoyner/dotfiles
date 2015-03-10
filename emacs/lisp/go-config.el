(require 'auto-complete)
(require 'go-autocomplete)
(require 'go-mode)

(defun my-go-mode-hook ()
  ; Turn on auto-complete and flycheck
  (auto-complete-mode 1)
  (if (featurep 'go-flycheck) (flycheck-mode 1))

  ; Call gofmt/goimports before saving
  (add-hook 'before-save-hook 'gofmt-before-save)

  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'go-mode-hook 'my-go-mode-hook)

(provide 'go-config)

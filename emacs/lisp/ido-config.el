(require 'djoyner-funcs)

;; Setup ido-mode
(setq ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-max-work-directory-list 0
      ido-max-work-file-list 0
      ido-record-commands nil
      ido-show-dot-for-dired t)

(defun my-ido-setup ()
  (define-key ido-file-dir-completion-map "~" 'djoyner/ido-jump-to-home)
  (define-key ido-file-dir-completion-map "/" 'djoyner/ido-smart-jump-to-root)
  (define-key ido-common-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-common-completion-map (kbd "C-k") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my-ido-setup)

(after 'ibuffer
  (define-key ibuffer-mode-map (kbd "C-x C-f") 'ido-find-file))

(ido-mode 1)

(provide 'ido-config)

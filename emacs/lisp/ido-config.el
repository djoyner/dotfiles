(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-last-directory-history nil
      ido-everywhere t
      ido-max-work-directory-list 0
      ido-max-work-file-list 0
      ido-record-commands nil
      ido-show-dot-for-dired t)

(defun my-ido-setup ()
  (define-key ido-file-dir-completion-map "~" 'djoyner/ido-jump-to-home)
  (define-key ido-file-dir-completion-map [tab] 'ido-complete)
  (define-key ido-file-dir-completion-map (kbd "RET") 'exit-minibuffer)
  (define-key ido-file-dir-completion-map (kbd "C-i") 'ido-select-text)
  (define-key ido-completion-map (kbd "RET") 'exit-minibuffer)
  (define-key ido-completion-map (kbd "TAB") 'ido-complete)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-i") 'ido-select-text)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match))

(add-hook 'ido-setup-hook 'my-ido-setup)

(ido-mode 1)

(provide 'ido-config)

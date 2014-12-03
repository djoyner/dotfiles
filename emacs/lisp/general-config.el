;; Re-enable various useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; One-character yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Miscellaneous settings
(setq debug-on-error nil
      win32-alt-is-meta nil)

(provide 'general-config)

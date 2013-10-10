;; Re-enable various useful commands that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Miscellaneous settings
(setq debug-on-error nil
      win32-alt-is-meta nil)

(provide 'general-config)

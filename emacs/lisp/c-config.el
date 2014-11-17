(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc$" . c++-mode))

(c-add-style "my-c-style"
	     '("bsd"
	       (tab-width . 8)
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 4)
	       (c-tab-always-indent . nil)
	       (c-hanging-braces-alist . ((brace-list-open)
					  (brace-list-close)
					  (brace-entry-open)
					  (brace-entry-close)
					  (statement-cont)
					  (block-close . c-snug-do-while)
					  (extern-lang-open after)
					  (namespace-open after)))
	       (c-hanging-colons-alist . ((member-init-intro before)
					  (inher-intro)
					  (case-label after)
					  (label after)
					  (access-label after)))
	       (c-cleanup-list . (scope-operator
				  defun-close-semi))))

(defun my-c-mode-hook ()
  (c-set-style "my-c-style")
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  (c-toggle-auto-hungry-state t))

(add-hook 'c-mode-common-hook 'my-c-mode-hook)

(provide 'c-config)

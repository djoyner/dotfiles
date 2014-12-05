(autoload 'js3-mode "js3-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js3-mode))

(setq js3-lazy-commas t
      js3-lazy-operators t
      js3-lazy-dots t
      js3-expr-indent-offset 2
      js3-paren-indent-offset 2
      js3-square-indent-offset 2
      js3-curly-indent-offset 2)

(provide 'js-config)

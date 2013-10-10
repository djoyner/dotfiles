;; Don't use add-hook here in case haskell-mode is globally installed and has already set some default hooks.
;; Better to just set our complete list of mode hooks that we want.
(setq haskell-mode-hook `(turn-on-haskell-indentation turn-on-haskell-doc-mode turn-on-haskell-decl-scan))

(provide 'haskell-config)

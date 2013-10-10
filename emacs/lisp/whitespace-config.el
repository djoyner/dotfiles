(require 'whitespace)

(setq whitespace-display-mappings
      '((space-mark   ?\     [?\u00B7]     [?.])	; space - centered dot
        (space-mark   ?\xA0  [?\u00A4]     [?_])	; hard space - currency
        (newline-mark ?\n    [?\u00AC ?\n] [?$ ?\n])	; eol - negation
        (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])	; tab - left quote mark
        ))

(provide 'whitespace-config)

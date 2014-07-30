(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)

(autoload 'powershell-mode "powershell-mode" "An editing mode for Microsoft Powershell." t)
(eval-after-load "powershell-mode" '(setq debug-on-error nil))
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

(provide 'powershell-config)

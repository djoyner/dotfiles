(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)
(autoload 'powershell-mode "powershell-mode" "An editing mode for Microsoft Powershell." t)

(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.psm\\'" . powershell-mode))

(after "powershell-mode"
  (setq debug-on-error nil))

(provide 'powershell-config)

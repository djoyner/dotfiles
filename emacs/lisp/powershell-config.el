;;; ~/emacs/lisp/powershell-config.el

(autoload 'powershell "powershell" "Run powershell as a shell within emacs." t)
(autoload 'powershell-mode "powershell-mode" "An editing mode for Microsoft Powershell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))

;;; end ~/emacs/lisp/powershell-config.el

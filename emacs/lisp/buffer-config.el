(require 'ibuffer)
(require 'uniquify)

;; Make buffer names unique using file path info
(setq uniquify-buffer-name-style 'forward
      uniquify-ignore-buffers-re "^\\*" ; leave special buffers alone
      uniquify-after-kill-buffer-p t)

;; Set up ibuffer
(setq ibuffer-saved-filter-groups '(("default"
				     ("c" (or (mode . c-mode)
                                              (mode . c++-mode)))
				     ("dired" (mode . dired-mode))
				     ("elisp" (mode . emacs-lisp-mode))
				     ("haskell" (or (mode . haskell-mode)
						    (mode . haskell-cabal-mode)))
				     ("python" (mode . python-mode))
				     ("xml" (mode . nxml-mode))
				     ("*magit*" (name . "^\\*magit"))
				     ("*man*" (name . "^\\*Man "))
				     ("*emacs*" (name . "\\*.*\\*"))))
      ibuffer-formats '((mark modified read-only " "
                              (name 40 40) " "
                              (size 6 -1 :right) " "
                              (mode 16 16 :left) " "
                              filename)
			(mark " " (name 16 -1) " "
                              filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&"
      ibuffer-show-empty-filter-groups nil)

(defun my-ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (ibuffer-auto-mode 1))

(add-hook 'ibuffer-mode-hook 'my-ibuffer-mode-hook)

;; Default to ibuffer
(defalias 'list-buffers 'ibuffer)

(provide 'buffer-config)

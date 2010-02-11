;;; ~/emacs/lisp/buffer-config.el

;; Make buffer names unique using file path info
(when (try-require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward))

;; Set up ibuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups '(("default"
				     ("c" (mode . c-mode))
				     ("c++" (mode . c++-mode))
				     ("dired" (mode . dired-mode))
				     ("elisp" (mode . emacs-lisp-mode))
				     ("haskell" (or (mode . haskell-mode)
						    (mode . haskell-cabal-mode)))
				     ("python" (mode . python-mode))
				     ("xml" (mode . nxml-mode))
				     ("*vc*" (or (name . "^\\*vc\\*$")
					       (name . "^\\*vc-")
					       (name . "^\\*Annotate")
					       (name . "^\\*git-")
					       (name . "^\\*P4")
					       (name . "^\\*Opened Files")))
				     ("*emacs*" (name . "\\*.*\\*"))))
      ibuffer-formats '((mark modified read-only " " (name 40 40) " " (size 6 -1 :right) " " (mode 16 16 :left) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&"
      ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; Set up iswitchb
(iswitchb-mode 1)
(setq iswitchb-default-method 'maybe-frame)

;; Remap common buffer bindings to ibuffer and iswitchb
(define-key global-map "\C-xb" 'iswitchb-buffer)
(define-key global-map "\C-x\C-b" 'ibuffer)

;;; end ~/emacs/lisp/buffer-config.el

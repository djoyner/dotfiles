(require 'haskell-mode)

;; Don't use add-hook here in case haskell-mode is globally installed and has already set some default hooks.
;; Better to just set our complete list of mode hooks that we want.
(setq haskell-mode-hook `(turn-on-haskell-indentation
                          turn-on-haskell-doc-mode
                          turn-on-haskell-decl-scan))

;; Autoload ghc-mod with flymake-mode syntax checking
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

;; Setup haskell-mode key bindings
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

;; Add cabal's bin directory to the exec-path
(if (file-exists-p "~/.cabal/bin")
    (add-to-list 'exec-path "~/.cabal/bin"))

(if (file-exists-p "~/Library/Haskell/bin")
    (add-to-list 'exec-path "~/Library/Haskell/bin"))

;; Other haskell-mode setup
(setq haskell-hoogle-command "hoogle"
      haskell-process-type 'cabal-repl
      haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-tags-on-save t)

(provide 'haskell-config)

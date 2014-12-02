(require 'flycheck)
(require 'flycheck-haskell)
(require 'haskell-mode)

;; Don't use add-hook here in case haskell-mode is globally installed and has already set some default hooks.
;; Better to just set our complete list of mode hooks that we want.
(setq haskell-mode-hook `(turn-on-haskell-indentation
                          turn-on-haskell-doc-mode
                          turn-on-haskell-decl-scan))

(defun my-haskell-mode-hook ()
  (auto-complete-mode 1)
  (flycheck-mode 1)
  (ghc-init))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; Use the flycheck-hdevtools checker
(after 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  (require 'flycheck-hdevtools))

;; Make Haskell modes play nice with evil
(after 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))

(after 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))

(after 'haskell-interactive-mode
  (evil-set-initial-state 'haskell-interactive-mode 'emacs))

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

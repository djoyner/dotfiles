;; $ cabal install ghc-mod hasktags stylish-haskell hoogle hlint

(require 'align)
(require 'flycheck)
(require 'flycheck-haskell)
(require 'haskell-mode)
(require 'rainbow-delimiters)

;; Don't use add-hook here in case haskell-mode is globally installed and has already set some default hooks.
;; Better to just set the list of mode hooks that we want.
(defun my-haskell-mode-hook ()
  (auto-complete-mode 1)
  (flycheck-mode 1)
  (rainbow-delimiters-mode 1)
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))

(setq haskell-mode-hook `(turn-on-haskell-indentation
                          turn-on-haskell-decl-scan
                          my-haskell-mode-hook))

;; Setup flycheck-haskell using both ghc and hlint
(after 'flycheck
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup)
  (flycheck-add-next-checker 'haskell-ghc '(warning . haskell-hlint)))

;; Setup haskell-mode bindings
(after 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-s") 'haskell-mode-stylish-buffer)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c i") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))

(after 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))

;; Add cabal's bin directory to the exec-path
(if (file-exists-p "~/.cabal/bin")
    (add-to-list 'exec-path (expand-file-name "~/.cabal/bin")))

(if (file-exists-p "~/Library/Haskell/bin")
    (add-to-list 'exec-path (expand-file-name "~/Library/Haskell/bin")))

;; Other haskell-mode setup
(setq haskell-hoogle-command "hoogle"
      haskell-indent-spaces 4
      haskell-indentation-ifte-offset 4
      haskell-indentation-layout-offset 4
      haskell-indentation-left-offset 4
      haskell-process-type 'cabal-repl
      haskell-process-auto-import-loaded-modules t
      haskell-process-show-debug-tips nil
      haskell-process-suggest-remove-import-lines t
      haskell-tags-on-save t)

(provide 'haskell-config)

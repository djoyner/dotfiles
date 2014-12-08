;; $ cabal install ghc-mod hasktags stylish-haskell hoogle hlint

(require 'align)
(require 'flycheck)
(require 'flycheck-haskell)
(require 'haskell-mode)
(require 'rainbow-delimiters)

(defconst haskell-unicode-conversions
  '(("[ (]\\(->\\)[) \n]"      . ?→)
    ("[ (]\\(/=\\)[) ]"        . ?≠)
    ("[ (]\\(<=\\)[) ]"        . ?≤)
    ("[ (]\\(>=\\)[) ]"        . ?≥)
    ;("[ (]\\(=\\)[) ]"        . ?≡)
    ("[ (]\\(\\.\\)[) ]"       . ?∘)
    ("[ (]\\(&&\\)[) ]"        . ?∧)
    ("[ (]\\(||\\)[) ]"        . ?∨)
    ("[ (]\\(\\*\\)[) ]"       . ?×)
    ("[ (]\\(\\\\\\)[(_a-z]"   . ?λ)
    (" \\(<-\\)[ \n]"          . ?←)
    ;(" \\(-<\\) "             . ?↢)
    ;(" \\(>-\\) "             . ?↣)
    (" \\(=>\\)[ \n]"          . ?⇒)
    ;;(" \\(>=>\\) "           . ?↣)
    ;;(" \\(<=<\\) "           . ?↢)
    ;;(" \\(>>=\\) "           . ?↦)
    ;;(" \\(=<<\\) "           . ?↤)
    ("[ (]\\(\\<not\\>\\)[ )]" . ?¬)
    ("[ (]\\(<<<\\)[ )]"       . ?⋘)
    ("[ (]\\(>>>\\)[ )]"       . ?⋙)
    (" \\(::\\) "              . ?∷)
    ("\\(`union`\\)"           . ?⋃)
    ("\\(`intersect`\\)"       . ?⋂)
    ("\\(`elem`\\)"            . ?∈)
    ("\\(`notElem`\\)"         . ?∉)
    ;;("\\<\\(mempty\\)\\>"    . ??)
    ;;("\\(`mappend`\\)"       . ?⨂)
    ;;("\\(`msum`\\)"          . ?⨁)
    ("\\(\\<undefined\\>\\)"   . ?⊥)
    ("\\<\\(forall \\)\\>"     . ?∀)))

(defun my-haskell-setup-unicode-conversions ()
  (interactive)
  (mapc (lambda (mode)
          (font-lock-add-keywords
           mode
           (append (mapcar (lambda (chars)
                             `(,(car chars)
                               ,(if (characterp (cdr chars))
                                    `(0 (ignore
                                         (compose-region (match-beginning 1)
                                                         (match-end 1)
                                                         ,(cdr chars))))
                                  `(0 ,(cdr chars)))))
                           haskell-unicode-conversions))))
        '(haskell-mode literate-haskell-mode)))

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
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (my-haskell-setup-unicode-conversions))

(after 'haskell-cabal
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile)
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal))

;; Add cabal's bin directory to the exec-path
(when (file-exists-p "~/.cabal/bin")
  (add-to-list 'exec-path (expand-file-name "~/.cabal/bin"))
  (when (file-exists-p "~/.cabal/bin/cabal")
    (setq haskell-process-path-cabal (expand-file-name "~/.cabal/bin/cabal")))
  (when (file-exists-p "~/.cabal/bin/hoogle")
    (setq haskell-hoogle-command (expand-file-name "~/.cabal/bin/hoogle"))))

(when (file-exists-p "~/Library/Haskell/bin")
  (add-to-list 'exec-path (expand-file-name "~/Library/Haskell/bin"))
  (when (file-exists-p "~/Library/Haskell/bin/cabal")
    (setq haskell-process-path-cabal (expand-file-name "~/Library/Haskell/bin/cabal")))
  (when (file-exists-p "~/Library/Haskell/bin/hoogle")
    (setq haskell-hoogle-command (expand-file-name "~/Library/Haskell/bin/hoogle"))))

;; Other haskell-mode setup
(setq haskell-indent-spaces 2
      haskell-indentation-ifte-offset 2
      haskell-indentation-layout-offset 2
      haskell-indentation-left-offset 2
      haskell-process-type 'cabal-repl
      haskell-process-auto-import-loaded-modules t
      haskell-process-show-debug-tips nil
      haskell-tags-on-save t)

(provide 'haskell-config)

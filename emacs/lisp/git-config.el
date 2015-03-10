(require 'magit)

;; Get rid of horrible git-rebase-mode
(setq auto-mode-alist (delete '("/git-rebase-todo\\'" . git-rebase-mode)
                              auto-mode-alist))

(provide 'git-config)

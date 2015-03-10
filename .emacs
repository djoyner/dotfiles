;; Add our elisp directories to the load-path
(add-to-list 'load-path (concat (getenv "HOME") "/emacs/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs/site-lisp") 'append)

;; The remainder of config is loaded from packages
(dolist (pkg '(init-packages
               init-server
               misc-funcs
               djoyner-funcs
               buffer-config
               c-config
               dired-config
               docker-config
               editing-config
               evil-config
               general-config
               git-config
               go-config
               haskell-config
               help-config
               ido-config
               js-config
               key-config
               lisp-config
               markdown-config
               minibuffer-config
               mswin-config
               powershell-config
               python-config
               screen-config
               shell-config
               skeleton-config
               xml-config
               yaml-config))
  (with-demoted-errors "Initialization error: %s"
    (require pkg)))

(when (file-exists-p "~/.emacs.local.el")
  (load-file "~/.emacs.local.el"))

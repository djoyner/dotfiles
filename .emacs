;; Add our elisp directories to the load-path
(add-to-list 'load-path (concat (getenv "HOME") "/emacs/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/emacs/site-lisp") 'append)

;; The remainder of config is loaded from packages
(dolist (pkg '(init-packages
               init-server
               djoyner-funcs
               misc-funcs
               buffer-config
               c-config
               dired-config
               docker-config
               editing-config
               evil-config
               general-config
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
               whitespace-config
               xml-config))
  (with-demoted-errors "Initialization error: %s"
    (require pkg)))

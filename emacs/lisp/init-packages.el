(require 'cl)
(require 'package)

(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar my-packages
  '(color-theme
    dockerfile-mode
    elisp-slime-nav
    evil
    evil-leader
    evil-matchit
    evil-surround
    evil-visualstar
    flycheck
    flycheck-haskell
    flycheck-hdevtools
    ghc
    ghci-completion
    gist
    go-autocomplete
    go-eldoc
    go-mode
    graphviz-dot-mode
    haskell-mode
    htmlize
    js2-mode
    magit
    markdown-mode
    org
    powershell
    powershell-mode
    yaml-mode
    zenburn-theme)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; Check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'init-packages)

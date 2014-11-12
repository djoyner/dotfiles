(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(defvar my-packages
  '(browse-kill-ring
    color-theme
    color-theme-solarized
    evil
    evil-leader
    evil-search-highlight-persist
    evil-visualstar
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
    surround
    yaml-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'package-config)

;;; ~/.emacs

;; Directory setup
(defvar my-root (getenv "HOME")
  "The root of my personal emacs world")

(defvar my-lisp-directory (concat my-root "/emacs/lisp")
  "Directory where my personal elisp files reside")

(defvar my-site-lisp-directory (concat my-root "/emacs/site-lisp")
  "Directory where additional elisp files (from teh interwebz) reside")

;; Add interesting elisp directories to the load-path
(add-to-list 'load-path my-lisp-directory)
(add-to-list 'load-path my-site-lisp-directory t)

(dolist (p '("erlang"		;; file:/usr/lib64/erlang/lib/tools-2.5.2/emacs
	     "haskell-mode"	;; http://projects.haskell.org/haskellmode-emacs
	     "nxml-mode"	;; http://www.thaiopensource.com/nxml-mode
	     "ruby-mode"	;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/ruby-mode
	     ))
  (add-to-list 'load-path (concat my-site-lisp-directory "/" p) t))

;; The remainder of my config is in libraries
(load-library "efuncs")				;; custom functions
(load-library "ekeys")				;; key bindings
(load-library "cc-config")			;; C/C++ mode config
(load-library "compile-config")			;; compile-related config
(load-library "dired-config")			;; dired-mode config
(load-library "erl-config")			;; Erlang mode config
(load-library "git-config")			;; Git mode config
(load-library "haskell-config")			;; Haskell mode config
(load-library "irc-config")			;; IRC client config
(load-library "misc-config")			;; miscellaneous one-off config settings
(load-library "p4-config")			;; Perforce config
(load-library "ruby-config")			;; Ruby mode config
(load-library "screen-config")			;; window config
(load-library "shell-config")			;; shell config
(load-library "skeleton-config")		;; skeleton config
(load-library "xml-config")			;; XML mode config

(server-start)					;; start the emacs server running

;;; end ~/.emacs

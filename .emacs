;;; ~/.emacs

(require 'cl)

;; I keep everything under ~/emacs
(defvar emacs-root (cond ((eq system-type 'cygwin) "/home/djoyner/")
			 ((eq system-type 'gnu/linux) "/home/djoyner/")
			 ((eq system-type 'linux) "/home/djoyner/")
			 ((eq system-type 'darwin) "/Users/david/")
			 (t "c:/home/djoyner/"))
  "My home directory -- the root of my personal emacs load-path")

;; Add all the elisp directories under ~/emacs to my load path
(labels ((add-path (p) (add-to-list 'load-path (concat emacs-root p))))
  (add-path "emacs/lisp")            		;; all my personal elisp code
  (add-path "emacs/site-lisp")			;; elisp stuff from the net
  (add-path "emacs/site-lisp/color-theme")	;; http://www.emacswiki.org/cgi-bin/wiki?ColorTheme
  (add-path "emacs/site-lisp/erlang")		;; file:/usr/lib64/erlang/lib/tools-2.5.2/emacs
  (add-path "emacs/site-lisp/git-emacs")	;; git://github.com/tsgates/git-emacs.git
  (add-path "emacs/site-lisp/nxml-mode")	;; http://www.thaiopensource.com/nxml-mode
  (add-path "emacs/site-lisp/speedbar")		;; http://cedet.sourceforge.net/speedbar.shtml
  )

;; The remainder of my config is in libraries
(load-library "efuncs")				;; custom functions
(load-library "ekeys")				;; key bindings
(load-library "screen-config")			;; window config for all platforms
(load-library "skeleton-config")		;; skeleton config
(load-library "irc-config")			;; IRC client config
(load-library "misc-config")			;; miscellaneous one-off config settings
(load-library "my-ccmode")			;; C/C++ mode config
(load-library "my-erlmode")			;; Erlang mode config
(load-library "my-xmlmode")			;; XML mode config
(load-library "my-p4mode")			;; Perforce mode config
(load-library "my-gitmode")			;; Git mode config
(load-library "my-rubymode")			;; Ruby mode config

(when (eq system-type 'windows-nt)
  (server-start)				;; start the emacs server running
  (global-set-key "\C-x\C-c" nil))		;; remove the binding of C-x C-c, which normally exits emacs

;;; end ~/.emacs

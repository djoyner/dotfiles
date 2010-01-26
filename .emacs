;;; ~/.emacs

;; Environment setup and platform-convenience macros
(defvar running-ms-windows
  (eq system-type `windows))

(defvar running-gnu-linux
  (eq system-type 'gnu/linux))

(defvar running-mac-osx
  (or (eq system-type 'darwin) (eq system-type 'ns)))

(defmacro when-ms-windows (&rest body)
  (list 'if running-ms-windows (cons 'progn body)))

(defmacro when-gnu-linux (&rest body)
  (list 'if running-gnu-linux (cons 'progn body)))

(defmacro when-mac-osx (&rest body)
  (list 'if running-mac-osx (cons 'progn body)))

(defvar my-root (getenv "HOME")
  "The root of my personal emacs world")

(defvar my-lisp-directory (concat my-root "/emacs/lisp")
  "Directory where my personal elisp files reside")

(defvar my-site-lisp-directory (concat my-root "/emacs/site-lisp")
  "Directory where additional elisp files (from teh interwebz) reside")

;; Add interesting elisp directories to the load-path
(add-to-list 'load-path my-lisp-directory)
(add-to-list 'load-path my-site-lisp-directory 'append)

(dolist (p '("erlang"		;; file:/usr/lib64/erlang/lib/tools-2.5.2/emacs
	     "haskell-mode"	;; http://projects.haskell.org/haskellmode-emacs
	     "nxml-mode"	;; http://www.thaiopensource.com/nxml-mode
	     "ruby-mode"	;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/ruby-mode
	     ))
  (add-to-list 'load-path (concat my-site-lisp-directory "/" p) 'append))

;; Attempt to load a feature/library, failing silently
(defvar try-require-missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      (if (stringp feature)
	  (load-library feature)
	(require feature))
    (file-error
     (progn
       (message "Checking for library `%s'... MISSING" feature)
       (add-to-list 'try-require-missing-packages-list feature 'append))
     nil)))

;; Automatically [re]compile elisp files as they are loaded
(when (try-require 'byte-code-cache)
    (require 'bytecomp)
    (add-to-list 'load-path bcc-cache-directory))

;; The remainder of config is loaded from libraries
(try-require "efuncs")				;; custom functions
(try-require "ekeys")				;; key bindings
(try-require "cc-config")			;; C/C++ mode config
(try-require "compile-config")			;; compile-related config
(try-require "dired-config")			;; dired-mode config
(try-require "erl-config")			;; Erlang mode config
(try-require "git-config")			;; Git mode config
(try-require "haskell-config")			;; Haskell mode config
(try-require "irc-config")			;; IRC client config
(try-require "misc-config")			;; miscellaneous one-off config settings
(try-require "p4-config")			;; Perforce config
(try-require "ruby-config")			;; Ruby mode config
(try-require "screen-config")			;; window config
(try-require "shell-config")			;; shell config
(try-require "skeleton-config")			;; skeleton config
(try-require "xml-config")			;; XML mode config

(server-start)					;; start the emacs server running

;; Display a warning if any packages failed to load
(when try-require-missing-packages-list
  (display-warning
   '.emacs
   (concat "One or more packages failed to load: "
	   (prin1-to-string try-require-missing-packages-list))
   :error))

;;; end ~/.emacs

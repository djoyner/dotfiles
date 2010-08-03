;;; ~/.emacs

;; Environment setup and platform-convenience macros
(defvar running-ms-windows
  (or (eq system-type `windows-nt) (eq system-type 'cygwin)))

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

(dolist (dir '("erlang"			;; file:/usr/lib64/erlang/lib/tools-2.5.2/emacs
	       "haskell-mode"		;; http://projects.haskell.org/haskellmode-emacs
	       "nxml-mode"		;; http://www.thaiopensource.com/nxml-mode
	       "ruby-mode"		;; http://svn.ruby-lang.org/repos/ruby/trunk/misc/ruby-mode
	       ))
  (add-to-list 'load-path (concat my-site-lisp-directory "/" dir) 'append))

;; Attempt to load a feature/library, failing silently
(defvar try-require-missing-packages-list nil
  "List of packages that `try-require' can't find.")

(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      (progn
	(if (stringp feature)
	    (load-library feature)
	  (require feature))
	t)
    (file-error
     (progn
       (message "try-require failed to load library `%s'" feature)
       (add-to-list 'try-require-missing-packages-list feature 'append))
     nil)))

;; Automatically [re]compile elisp files as they are loaded
(when (try-require 'byte-code-cache)
    (require 'bytecomp)
    (add-to-list 'load-path bcc-cache-directory))

;; The remainder of config is loaded from libraries
(dolist (lib '("buffer-config"		;; buffer-related config
	       "cc-config"		;; C/C++ mode config
	       "compile-config"		;; compile-related config
	       "cygwin-config"		;; Cygwin-related config
	       "dired-config"		;; dired-mode config
	       "editing-config"		;; editing-related config
	       "erl-config"		;; erlang-mode config
	       "general-config"		;; general configuration settings
	       "git-config"		;; git-mode config
	       "haskell-config"		;; haskell-mode config
	       "help-config"		;; help-related config
	       "irc-config"		;; IRC client config
	       "key-config"		;; general key bindings
	       "lisp-config"		;; lisp-related config
	       "minibuffer-config"	;; minibuffer-related config
	       "misc-funcs"		;; miscellaneous elisp functions
	       "p4-config"		;; p4 config
	       "ruby-config"		;; ruby-mode config
	       "screen-config"		;; screen-related config
	       "shell-config"		;; shell-mode config
	       "skeleton-config"	;; skeleton config
	       "twitter-config"		;; twitter config
	       "xml-config"		;; xml-mode config
	       ))
  (try-require lib))

;; Start the emacs server running as long as I'm not root
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (server-start))

;; Display a warning if any packages failed to load
(when try-require-missing-packages-list
  (display-warning
   '.emacs
   (concat "One or more packages failed to load: "
	   (prin1-to-string try-require-missing-packages-list))
   :error))

;;; end ~/.emacs

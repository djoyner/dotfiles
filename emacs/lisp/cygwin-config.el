;;; ~/emacs/lisp/cygwin-config.el

(when-ms-windows
 (load-library "cl-extra")

 ;; Add Cygwin bin directory to emacs' internal exec-path and to environment's PATH if not already present
 (let* ((cygwin-bin-path "c:/cygwin/bin")
        (cygwin-bin-dos-path (replace-regexp-in-string "/" "\\\\" cygwin-bin-path))
        (env-path (split-string (getenv "PATH") ";")))
   (add-to-list 'exec-path cygwin-bin-path t 'equalp)
   (setenv "PATH" (mapconcat
                   (lambda (str) (concat str ";"))
                   (add-to-list 'env-path cygwin-bin-dos-path t 'equalp) '())))

 ;; LOGNAME and USER are expected in many Emacs packages
 (if (and (null (getenv "USER"))
          (getenv "USERNAME"))
     (setenv "USER" (getenv "USERNAME")))

 (if (and (getenv "LOGNAME")
          (null (getenv "USER")))
     (setenv "USER" (getenv "LOGNAME")))

 (if (and (getenv "USER")
          (null (getenv "LOGNAME")))
     (setenv "LOGNAME" (getenv "USER")))

 ;; Add support for Cygwin mount table
 (when (try-require `cygwin-mount)
   (cygwin-mount-activate))

 ;; Disable file-name-shadow-mode
 (file-name-shadow-mode nil))

;;; end ~/emacs/lisp/cygwin-config.el

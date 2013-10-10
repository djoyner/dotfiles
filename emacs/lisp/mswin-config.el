(when-ms-windows
 ;; LOGNAME and USER are expected in many Emacs packages
 (if (and (null (getenv "USER"))
          (getenv "USERNAME"))
     (setenv "USER" (getenv "USERNAME")))

 (if (and (getenv "LOGNAME")
          (null (getenv "USER")))
     (setenv "USER" (getenv "LOGNAME")))

 (if (and (getenv "USER")
          (null (getenv "LOGNAME")))
     (setenv "LOGNAME" (getenv "USER"))))

(provide 'mswin-config)

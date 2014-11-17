(require 'server)

;; Start the emacs server running as long as I'm not root
(or (string-equal "root" (getenv "USER"))
    (server-running-p)
    (server-start))

(provide 'init-server)

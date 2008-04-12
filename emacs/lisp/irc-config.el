;;; ~/emacs/lisp/irc-config.el

(require 'rcirc)

(setq rcirc-default-server "kubrick.freenode.net"
      rcirc-default-nick "djoyner805"
      rcirc-default-user-full-name "djoyner805"
      rcirc-startup-channels-alist '(("\\.freenode\\.net$" "#erlang" "#emacs")))

;; (setq rcirc-authinfo
;;       '(("freenode" nickserv "YOURNICKNAME" "YOURPASSWD")))

(add-hook 'rcirc-mode-hook
	  (lambda () (rcirc-track-minor-mode 1)))

(add-hook 'rcirc-mode-hook
	  (lambda () (flyspell-mode 1)))

(eval-after-load 'rcirc
  '(add-to-list 'rcirc-markup-text-functions
		'rcirc-smileys))

(defun rcirc-smileys (&rest ignore)
  "Run smiley-buffer on the buffer but add a temporary space at the end
to ensure matches of smiley regular expressions."
  (goto-char (point-max))
  (insert " ")
  (smiley-buffer)
  (delete-char -1))

;;; end ~/emacs/lisp/irc-config.el

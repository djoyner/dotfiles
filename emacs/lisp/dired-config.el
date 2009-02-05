;;; ~/emacs/lisp/dired-config.el

(require 'dired)

; remap 'o' in dired mode to open a file
(defun dired-open-mac ()
  (interactive)
  (let ((file-name (dired-get-file-for-visit)))
    (if (file-exists-p file-name)
        (call-process "/usr/bin/open" nil 0 nil file-name))))

(when (eq system-type 'darwin)
  (define-key dired-mode-map "o" 'dired-open-mac))

;;; end ~/emacs/lisp/dired-config.el

(require 'dired)

;; Recursive deletes allowed, after asking for each directory at top level
(setq dired-recursive-deletes 'top)

;; Copy recursively without asking
(setq dired-recursive-copies 'always)

;; Remap 'o' in dired mode to open a file
(when-mac-osx
 (defun dired-open-mac ()
   (interactive)
   (let ((file-name (dired-get-file-for-visit)))
     (if (file-exists-p file-name)
	 (call-process "/usr/bin/open" nil 0 nil file-name))))
 (define-key dired-mode-map "o" 'dired-open-mac))

(provide 'dired-config)

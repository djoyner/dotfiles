(setq djoyner-ibuffer-packages '(ibuffer))

(defun djoyner-ibuffer/pre-init-ibuffer ()
  (with-eval-after-load 'ibuffer
    (require 'projectile)
    (setq ibuffer-saved-filter-groups (list (cons "default"
                                                  (append
                                                   (mapcar (lambda (it)
                                                             (let ((name (file-name-nondirectory
                                                                          (directory-file-name it))))
                                                               `(,name (filename . ,(expand-file-name it)))))
                                                           (sort (copy-sequence projectile-known-projects)
                                                                 (lambda (p1 p2) (cond
                                                                                  ((string= p1 "~/") nil)
                                                                                  ((string= p2 "~/") 't)
                                                                                  (t (string< p1 p2))))))
                                                   `(("Org" (mode . org-mode))
                                                     ("Dired" (mode . dired-mode))
                                                     ("IRC" (mode . erc-mode))
                                                     ("Emacs" (or (name . "\\*Messages\\*")
                                                                  (name . "\\*Compile-Log\\*")
                                                                  (name . "\\*scratch\\*")
                                                                  (name . "\\*spacemacs\\*")
                                                                  (name . "\\*emacs\\*")))
                                                     ("Magit" (name . "\\*magit"))
                                                     ("Help" (name . "\\*Help\\*"))
                                                     ("Helm" (name . "\\*helm"))
                                                     ))))
          ibuffer-formats '((mark modified read-only " "
                                  (name 40 40) " "
                                  (size 6 -1 :right) " "
                                  (mode 16 16 :left) " "
                                  filename)
                            (mark " " (name 16 -1) " "
                                  filename))
          ibuffer-elide-long-columns t
          ibuffer-eliding-string "&"
          ibuffer-show-empty-filter-groups nil)
    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")
                (ibuffer-auto-mode t)))))

;;; ~/emacs/lisp/my-ccmode.el

(defconst my-c-style
  '((c-tab-always-indent        . nil)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open . 0)
                                   (case-label        . 2)
                                   (block-open        . 0)
                                   (comment-intro     . 0)
                                   (member-init-intro . 4)
                                   (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t)
    ) "Dave's C/C++ Programming Style")

;; Offset customizations not in my-c-style
(setq c-offsets-alist '((member-init-intro . ++)))

;; Customizations for all modes in CC Mode
(defun my-c-mode-common-hook ()
  ;; Add my personal style and set it for the current buffer
  (c-add-style "Personal" my-c-style t)

  ;; Other customizations
  (setq tab-width 8
        indent-tabs-mode nil)

  ;; I like auto-newline and hungry-delete
  (c-toggle-auto-hungry-state 1)

  ;; Key bindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, idl-mode-map, and pike-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'c-context-line-break)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; end ~/emacs/lisp/my-ccmode.el

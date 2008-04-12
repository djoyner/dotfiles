;;; ~/emacs/lisp/my-xnlmode.el

(load-library "rng-auto")

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|idl\\)\\'" . nxml-mode)
	    auto-mode-alist))

;;; end ~/emacs/lisp/my-xmlmode.el

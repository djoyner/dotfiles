;;; ~/emacs/lisp/xml-config.el

(load-library "rng-auto")

(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|idl\\)\\'" . nxml-mode)
	    auto-mode-alist))

;;; end ~/emacs/lisp/xml-config.el

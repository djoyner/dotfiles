;;; ~/emacs/lisp/xml-config.el

(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|idl\\)\\'" . nxml-mode))
(fset 'xml-mode 'nxml-mode)

;;; end ~/emacs/lisp/xml-config.el

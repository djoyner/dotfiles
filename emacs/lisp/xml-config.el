(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|idl\\)\\'" . nxml-mode))
(fset 'xml-mode 'nxml-mode)

(provide 'xml-config)

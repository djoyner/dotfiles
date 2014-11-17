(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|idl\\)\\'" . nxml-mode))
(defalias 'xml-mode 'nxml-mode)

(provide 'xml-config)

(defun djoyner/copy-to-end-of-line ()
  (interactive)
  (copy-region-as-kill (point) (point-at-eol)))

(provide 'djoyner-funcs)

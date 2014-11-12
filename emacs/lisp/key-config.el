;; Remap global keys
(define-key global-map "\C-h" 'delete-backward-char)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [del] 'delete-char)
(global-set-key [M-up] 'move-line-up)
(global-set-key [M-down] 'move-line-down)

(global-set-key [S-f1] 'man)
(global-set-key [f8] 'next-error)
(global-set-key [S-f8] 'previous-error)
(global-set-key [f9] 'compile)
(global-set-key [S-f9] 'recompile)
(global-set-key [f10] 'bash)

;; Setup additional bindings for windowed systems
(defun logitech-mx-mouse-present-p ()
  "Returns t when Logitech Performance Mouse MX is present."
  (interactive)
  (ignore-errors (eq 0 (call-process "lsusb" nil nil nil "-d" "046d:c52b"))))

(when window-system
  ; Change the behavior of mouse yanks, so that they insert the selection at point
  ; (where the text cursor is), instead of at the position clicked
  (setq mouse-yank-at-point t)

  ; Setup additional mouse button bindings when Logitech Performance Mouse MX is present
  (when (logitech-mx-mouse-present-p)
    (global-set-key [mouse-8] 'end-of-buffer)		;; "down arrow"
    (global-set-key [mouse-9] 'beginning-of-buffer)	;; "up arrow"
    (global-set-key [mouse-13] 'delete-other-windows)  	;; "zoom"
    (global-set-key [mouse-10] 'ibuffer)) 		;; "window list"
  )

(provide 'key-config)

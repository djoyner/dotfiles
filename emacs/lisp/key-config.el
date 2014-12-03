(require 'misc-funcs)

;; Remap global keys
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-g") 'magit-status)
(global-set-key (kbd "M-[") 'align-code)

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

;; Make Mac modifier keys work sensibly:
;;  Command/⌘ = Meta
;;  Option/Alt = Super
;;  Fn = Nothing
(when-mac-osx
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super
        ns-function-modifier nil))

(provide 'key-config)

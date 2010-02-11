;;; ~/emacs/lisp/key-config.el

;; Remap global keys
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-x " 'just-one-space)
(define-key global-map "\M- " 'set-mark-command)
(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-=" 'what-line)

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [del] 'delete-char)
(global-set-key [M-up] 'move-line-up)
(global-set-key [M-down] 'move-line-down)

(global-set-key [S-f1] 'man)
(global-set-key [f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [f4] 'kmacro-end-or-call-macro)
(global-set-key [f5] 'undo)
(when (try-require `redo)
  (global-set-key [S-f5] 'redo))
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'next-buffer)
(global-set-key [S-f7] 'previous-buffer)
(global-set-key [f8] 'next-error)
(global-set-key [S-f8] 'previous-error)
(global-set-key [f9] 'compile)
(global-set-key [S-f9] 'recompile)
(global-set-key [f10] 'bash)
(global-set-key [f11] 'grep)
(global-set-key [S-f11] 'rgrep)
(global-set-key [f12] 'ibuffer)

;; Setup additional bindings for windowed systems
(defun logitech-mx-mouse-present-p ()
  "Returns t when Logitech Performance Mouse MX is present."
  (interactive)
  (ignore-errors (eq 0 (call-process "lsusb" nil nil nil "-d" "046d:c52b"))))

(when window-system
  ; Change the behavior of mouse yanks, so that they insert the selection at point
  ; (where the text cursor is), instead of at the position clicked
  (setq mouse-yank-at-point t)

  ; Make kill/yank interact with the clipboard
  (global-set-key "\C-w" 'clipboard-kill-region)
  (global-set-key "\M-w" 'clipboard-kill-ring-save)
  (global-set-key "\C-y" 'clipboard-yank)

  ; Setup additional mouse button bindings when Logitech Performance Mouse MX is present
  (when (logitech-mx-mouse-present-p)
    (global-set-key [mouse-8] 'end-of-buffer)		;; "down arrow"
    (global-set-key [mouse-9] 'beginning-of-buffer)	;; "up arrow"
    (global-set-key [mouse-13] 'delete-other-windows)  	;; "zoom"
    (global-set-key [mouse-10] 'ibuffer)) 		;; "window list"
  )

;; Put personal favorites in the CTRL-O keymap
(defvar ctrl-o-map (make-keymap)
  "Keymap for subcommands of C-o.")

(fset 'ctrl-o-prefix ctrl-o-map)
(define-key global-map "\^o" 'ctrl-o-prefix)
(define-key ctrl-o-map "a" 'auto-fill-mode)
(define-key ctrl-o-map "c" 'shell-clear-region)
(define-key ctrl-o-map "f" 'c-mark-function)
(define-key ctrl-o-map "g" 'gist-region)
(define-key ctrl-o-map "m" 'compile)
(define-key ctrl-o-map "p" 'pastie-region)
(define-key ctrl-o-map "r" 'query-replace-regexp)
(define-key ctrl-o-map "t" 'twitter-get-friends-timeline)
(define-key ctrl-o-map "w" 'delete-rectangle)
(define-key ctrl-o-map "\C-o" 'open-line)
(define-key ctrl-o-map "\C-?" 'delete-trailing-whitespace)
(define-key ctrl-o-map [down] 'throw-region)

;;; end ~/emacs/lisp/key-config.el

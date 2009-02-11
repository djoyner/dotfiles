;;; ~/emacs/lisp/ekeys.el

;; Remap global keys
(define-key global-map "\C-h" 'delete-backward-char)
(define-key global-map "\C-xb" 'iswitchb-buffer)
(define-key global-map "\C-x\C-b" 'ibuffer)
(define-key global-map "\C-x " 'just-one-space)
(define-key global-map "\M- " 'set-mark-command)
(define-key global-map "\M-g" 'goto-line)
(define-key global-map "\M-=" 'what-line)
(define-key global-map [(control x) return] nil)	;; make `C-x C-m' and `C-x RET' different

(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key [del] 'delete-char)
(global-set-key [f5] 'undo)
(global-set-key [f6] "\C-x\C-s\C-x0")

;; Change the binding of mouse button 2, so that it inserts the selection at point 
;; (where the text cursor is), instead of at the position clicked
;; (and window-system 
;;     ((define-key global-map 'button2 'x-insert-selection)))

;; Put personal favorites in the CTRL-O keymap
(defvar ctrl-o-map (make-keymap) 
  "Keymap for subcommands of C-o.")

(fset 'ctrl-o-prefix ctrl-o-map)
(define-key global-map "\^o" 'ctrl-o-prefix)
(define-key ctrl-o-map "a" 'auto-fill-mode)
(define-key ctrl-o-map "c" 'shell-clear-region)
(define-key ctrl-o-map "f" 'c-mark-function)
(define-key ctrl-o-map "g" 'goto-line)
(define-key ctrl-o-map "h" 'hscroll-mode)
(define-key ctrl-o-map "m" 'compile)
(define-key ctrl-o-map "p" 'pastie-region)
(define-key ctrl-o-map "r" 'query-replace-regexp)
(define-key ctrl-o-map "t" 'toggle-selective-display)
(define-key ctrl-o-map "w" 'delete-rectangle)
(define-key ctrl-o-map "\C-o" 'open-line)

;;; end ~/emacs/lisp/ekeys.el

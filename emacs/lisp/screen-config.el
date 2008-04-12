;;; ~/emacs/lisp/screen-config.el

;; Remove menubar, toolbar and scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Additional customization for X or Windows
(and window-system 
    (progn 

      ;; Turn on the color works
      (require 'color-theme)
      (color-theme-initialize)
      (setq color-theme-is-global t)
      (color-theme-aalto-light)
      (global-font-lock-mode t)

      ;; Disable blinking cursor
      (blink-cursor-mode nil)

      ;; Drive out the mouse when it's too near to the cursor
      (mouse-avoidance-mode 'animate)

      ;; Setup frame attributes
      (setq default-frame-alist (append '((vertical-scroll-bars . nil) 
					  (background-color . "#FFFFE0")
					  (cursor-color . "black") 
					  (cursor-type . box)
					  (width . 132)
					  (height . 60)
					  ) default-frame-alist))

      (cond ((eq window-system 'w32) (setq default-frame-alist (append '((font . "fixed613")) default-frame-alist)))
	    ((eq window-system 'x) (setq default-frame-alist (append '((font . "fixed")) default-frame-alist))))

      ;; Spruce up the title bar and mode line
      (setq frame-title-format '("%b (" system-name ")")
	    line-number-mode t
	    column-number-mode t
	    )
      ))

;; Miscellaneous stuff, regardless of display type
(setq ring-bell-function 'ignore
      scroll-step 1
      )

;;; end ~/emacs/lisp/screen-config.el

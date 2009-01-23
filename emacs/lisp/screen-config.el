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
      (global-font-lock-mode t)

      (defun color-theme-wombat ()
	(interactive)
	(color-theme-install
	 '(color-theme-wombat
	   ((foreground-color . "#f6f3e8")
	    (background-color . "#242424")
	    (cursor-color . "#656565")
	    (border-color . "#000000")
	    (background-mode . dark))
	   (default ((t (nil))))
	   (underline ((t (:underline t))))
	   (italic ((t (:italic t))))
	   (bold ((t (:bold))))
	   (bold-italic ((t (:bold t :italic t))))
	   (region ((t (:foreground "#000000" :background "#cae682"))))
	   (modeline ((t (:foreground "#f6f3e8" :background "#444444"))))
	   (modeline-buffer-id ((t (:foreground "#f6f3e8" :background "#444444"))))
	   (modeline-mousable ((t (:foreground "#f6f3e8" :background "#444444"))))
	   (modeline-mousable-minor-mode ((t (:foreground "#f6f3e8" :background "#444444"))))
	   (highlight-current-line-face ((t (:background "#2d2d2d"))))
	   (minibuffer-prompt ((t (:foreground "orange"))))
	   (font-lock-builtin-face ((t (:foreground "#8ac6f2"))))
	   (font-lock-comment-face ((t (:foreground "#99968b"))))
	   (font-lock-constant-face ((t (:foreground "#e5786d"))))
	   (font-lock-function-name-face ((t (:foreground "#cae682"))))
	   (font-lock-keyword-face ((t (:foreground "#8ac6f2"))))
	   (font-lock-preprocessor-face ((t (:foreground "#e5786d"))))
	   (font-lock-string-face ((t (:foreground "#95e454"))))
	   (font-lock-type-face ((t (:foreground "#cae682"))))
	   (font-lock-variable-name-face ((t (:foreground "#cae682"))))
	   (font-lock-warning-face ((t (:foreground "#e5786d" :bold t))))
	   (show-paren-match-face ((t (:foreground "#f6f3e8" :background "#857b6f" :bold t))))
	   (show-paren-mismatch-face ((t (:foreground "#e5786d" :background "#857b6f" :bold t)))))))

      (color-theme-wombat)

      (require 'highlight-current-line)
      (highlight-current-line-on t)

      ;; Disable blinking cursor
      (blink-cursor-mode nil)

      ;; Drive out the mouse when it's too near to the cursor
      (mouse-avoidance-mode 'animate)

      ;; Setup frame attributes
      (setq default-frame-alist (append '((vertical-scroll-bars . nil) 
					  (cursor-type . box)
					  (width . 180)
					  (height . 60)) default-frame-alist))

      (cond ((eq window-system 'w32) (setq default-frame-alist (append '((font . "fixed613")) default-frame-alist)))
	    ((eq window-system 'x) (setq default-frame-alist (append '((font . "fixed")) default-frame-alist))))

      ;; Spruce up the title bar and mode line
      (setq frame-title-format '("%b (" system-name ")"))))

;; Miscellaneous stuff, regardless of display type
(setq ring-bell-function 'ignore
      line-number-mode t
      column-number-mode t
      scroll-step 1)

;;; end ~/emacs/lisp/screen-config.el

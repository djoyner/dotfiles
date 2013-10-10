;; Remove menubar, toolbar and scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn on the color works
(require 'color-theme)
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
     (paren-face-match-light ((t (:background "#857b6f"))))
     (show-paren-match-face ((t (:foreground "#f6f3e8" :background "#857b6f" :bold t))))
     (show-paren-mismatch-face ((t (:foreground "#e5786d" :background "#857b6f" :bold t))))
     (twitter-header-face ((t (:background "#8a4513")))))))

(color-theme-wombat)

;; Highlight the current line
(when (try-require 'highlight-current-line)
  (highlight-current-line-on t))

;; Turn on line numbering
(when (try-require 'linum)
  (global-linum-mode t))

;; Show matching parens
(show-paren-mode t)

;; Show trailing whitespace
(setq-default show-trailing-whitespace t)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Drive out the mouse when it's too near to the cursor
(mouse-avoidance-mode 'animate)

;; Setup frame attributes
(setq default-frame-alist (append '((vertical-scroll-bars . nil)
                                    (cursor-type . box)
                                    (width . 180)
                                    (height . 55)) default-frame-alist))

;; Setup Consolas as the default font with platform-specific fallback to something reasonable
(defun font-family-exists-p (font)
  (if (and (fboundp 'font-spec) (null (list-fonts (font-spec :family font))))
      nil
    t))

(when-ms-windows
 (if (font-family-exists-p "Consolas")
     (setq default-frame-alist (append '((font . "-microsoft-Consolas-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")) default-frame-alist))
   (setq default-frame-alist (append '((font . "-outline-Courier New-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")) default-frame-alist))))

(when-gnu-linux
 (if (font-family-exists-p "Consolas")
     (setq default-frame-alist (append '((font . "Consolas-9")) default-frame-alist))
   (setq default-frame-alist (append '((font . "DejaVu Sans Mono-9")) default-frame-alist))))

(when-mac-osx
 (if (font-family-exists-p "Consolas")
     (setq default-frame-alist (append '((font . "-adobe-source code pro-medium-r-normal-*-11-*-*-*-m-*-iso10646-1")) default-frame-alist))
   (setq default-frame-alist (append '((font . "fixed")) default-frame-alist))))

;; Spruce up the title bar
(setq frame-title-format '("%b (" system-name ")"))

;; Other screen-related settings
(setq inhibit-splash-screen t
      ring-bell-function 'ignore
      line-number-mode t
      column-number-mode t
      scroll-preserve-screen-position t
      scroll-step 1
      temp-buffer-resize-mode t
      echo-keystrokes 0.1)

(provide 'screen-config)

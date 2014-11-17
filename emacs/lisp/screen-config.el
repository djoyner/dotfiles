(require 'color-theme)
(require 'linum)
(require 'zenburn-theme)

;; Remove menubar, toolbar and scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn on the color works
(load-theme 'zenburn t)
(global-font-lock-mode t)

;; Highlight the current line
;(when (try-require 'highlight-current-line)
;  (highlight-current-line-on t))

;; Turn on line numbering
 (global-linum-mode t)

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

(defun retina-display-p ()
  (condition-case nil
      (= (call-process "~/bin/is-retina-display") 0)
    (error nil)))

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
     (if (retina-display-p)
         (setq default-frame-alist (append '((font . "-microsoft-Consolas-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")) default-frame-alist))
       (setq default-frame-alist (append '((font . "-microsoft-Consolas-normal-normal-normal-*-10-*-*-*-m-0-iso10646-1")) default-frame-alist))
     (setq default-frame-alist (append '((font . "fixed")) default-frame-alist)))))

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

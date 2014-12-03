(require 'color-theme)
(require 'djoyner-funcs)
(require 'misc-funcs)
(require 'whitespace)
(require 'zenburn-theme)

;; Remove menubar, toolbar and scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn on the color works
(load-theme 'zenburn t)
(global-font-lock-mode t)

;; Show matching parens
(show-paren-mode t)

;; Whitespace display mappings
(setq whitespace-display-mappings
      '((space-mark   ?\     [?\u00B7]     [?.])	; space - centered dot
        (space-mark   ?\xA0  [?\u00A4]     [?_])	; hard space - currency
        (newline-mark ?\n    [?\u00AC ?\n] [?$ ?\n])	; eol - negation
        (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t])	; tab - left quote mark
        ))

;; Window-system customizations
(when window-system
  ; Disable blinking cursor
  (blink-cursor-mode 0)

  ; Drive out the mouse when it's too near to the cursor
  (mouse-avoidance-mode 'animate)

  ; Setup frame attributes
  (setq default-frame-alist (append '((vertical-scroll-bars . nil)
                                      (cursor-type . box)
                                      (width . 100)
                                      (height . 45)) default-frame-alist)
        frame-title-format '(buffer-file-name "%f" ("%b")))

  ; Setup Consolas as the default font with platform-specific fallback to something reasonable
  (when-ms-windows
     (setq default-frame-alist (append '((font . "-*-Consolas-*-*-*-*-13-*-*-*-*-*-iso10646-1")) default-frame-alist)))

  (when-gnu-linux
     (setq default-frame-alist (append '((font . "Consolas-9")) default-frame-alist)))

  (when-mac-osx
     (if (djoyner/retina-display-p)
         (setq default-frame-alist (append '((font . "-*-Consolas-*-*-*-*-11-*-*-*-*-*-iso10646-1")) default-frame-alist))
       (setq default-frame-alist (append '((font . "-*-Consolas-*-*-*-*-10-*-*-*-*-*-iso10646-1")) default-frame-alist)))))

;; Other screen-related settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      line-number-mode t
      column-number-mode t
      scroll-preserve-screen-position t
      scroll-step 1
      temp-buffer-resize-mode t
      echo-keystrokes 0.1
      use-dialog-box nil)

(setq-default show-trailing-whitespace t)

(provide 'screen-config)

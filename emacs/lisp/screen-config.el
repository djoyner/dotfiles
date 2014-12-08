(require 'color-theme)
(require 'djoyner-funcs)
(require 'git-gutter-fringe)
(require 'misc-funcs)
(require 'whitespace)
(require 'zenburn-theme)

;; Remove the menubar
(menu-bar-mode -1)

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
  ; Turn off toolbar scrollbars
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ; Disable blinking cursor
  (blink-cursor-mode 0)

  ; Setup git diffs in the gutter
  (global-git-gutter-mode 1)

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
    (let ((height (if (djoyner/retina-display-p) 110 100)))
      (set-face-attribute 'default nil :family "Consolas" :height height :weight 'normal :width 'normal)
      (set-fontset-font "fontset-default" 'unicode (font-spec :family "Arial Unicode MS" :size (* height .085) :width 'normal :weight 'normal)))))

;; Other screen-related settings
(setq inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      line-number-mode t
      column-number-mode t
      scroll-conservatively 10000
      scroll-margin 0
      scroll-preserve-screen-position t
      temp-buffer-resize-mode t
      echo-keystrokes 0.1
      use-dialog-box nil)

(setq-default show-trailing-whitespace t)

(provide 'screen-config)

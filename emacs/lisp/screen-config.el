;;; ~/emacs/lisp/screen-config.el

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

(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
        (base00  "#657b83")
        (base0   "#839496")
        (base1   "#93a1a1")
        (base2   "#eee8d5")
        (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (green   "#859900"))
    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0))
    (color-theme-install
     `(color-theme-solarized
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode)
        (cursor-color . ,base0))
       ;; basic
       (default ((t (:foreground ,base0))))
       (cursor ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
       (escape-glyph-face ((t (:foreground ,red))))
       (fringe ((t (:foreground ,base01 :background ,base02))))
       (header-line ((t (:foreground ,base0 :background ,base2))))
       (highlight ((t (:background ,base02))))
       (isearch ((t (:foreground ,yellow :inverse-video t))))
       (menu ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (mode-line
        ((t (:foreground ,base1 :background ,base02
                         :box (:line-width 1 :color ,base1)))))
       (mode-line-buffer-id ((t (:foreground ,base1))))
       (mode-line-inactive
        ((t (:foreground ,base0  :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       (region ((t (:background ,base01))))
       (secondary-selection ((t (:background ,base02))))
       (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
       (vertical-border ((t (:foreground ,base0))))
       ;; compilation
       (compilation-info ((t (:forground ,green :bold t))))
       (compilation-warning ((t (:foreground ,orange :bold t))))
       ;; customize
       (custom-button
        ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed
        ((t (:inherit custom-button-mouse
                      :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-documentation ((t (:inherit default))))
       (custom-group-tag ((t (:foreground ,orange :bold t))))
       (custom-link ((t (:foreground ,violet))))
       (custom-state ((t (:foreground ,green))))
       (custom-variable-tag ((t (:foreground ,orange :bold t))))
       ;; diff
       (diff-added ((t (:foreground ,green :inverse-video t))))
       (diff-changed ((t (:foreground ,yellow :inverse-video t))))
       (diff-removed ((t (:foreground ,red :inverse-video t))))
       ;; emacs-wiki
       (emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
       (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
       (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))
       ;; font-lock
       (font-lock-builtin-face ((t (:foreground ,green))))
       (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
       (font-lock-constant-face ((t (:foreground ,cyan))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face ((t (:foreground ,green))))
       (font-lock-string-face ((t (:foreground ,cyan))))
       (font-lock-type-face ((t (:foregound ,yellow))))
       (font-lock-variable-name-face ((t (:foregound ,blue))))
       (font-lock-warning-face ((t (:foreground ,red :bold t))))
       ;; info
       (info-xref ((t (:foreground ,blue :underline t))))
       (info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))
       ;; org
       (org-hide ((t (:foreground ,base03))))
       (org-todo ((t (:foreground ,red :bold t))))
       (org-done ((t (:foreground ,green :bold t))))
       ;; show-paren
       (show-paren-match-face ((t (:background ,cyan :foreground ,base3))))
       (show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))))))

(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

(color-theme-wombat)

;; Highlight the current line
(when (try-require 'highlight-current-line)
  (highlight-current-line-on t))

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
                                    (height . 60)) default-frame-alist))

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
     (setq default-frame-alist (append '((font . "-apple-Consolas-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")) default-frame-alist))
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

;;; end ~/emacs/lisp/screen-config.el

;;; ~/emacs/lisp/editing-config.el

;; Make text-mode the default major mode
(setq default-major-mode 'text-mode)

;; I want to be able to see the mark region and type over the selection
(transient-mark-mode t)
(delete-selection-mode t)

;; Change cutting behavior:
;; "Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea stolen
;; from Slickedit, if you press copy or cut when no region is active, you'll
;; copy or cut the current line."  <http://www.zafar.se/bkz/Articles/EmacsTips>
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Auto-indent pasted code
;; (defadvice yank (after indent-region activate)
;;   (if (member major-mode
;;               '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
;;                 objc-mode latex-mode plain-tex-mode python-mode))
;;       (indent-region (region-beginning) (region-end) nil)))

;; (defadvice yank-pop (after indent-region activate)
;;   (if (member major-mode
;;               '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
;;                 objc-mode latex-mode plain-tex-mode python-mode))
;;       (indent-region (region-beginning) (region-end) nil)))

;; Interactively insert items from the kill ring
(when (try-require 'browse-kill-ring)

  ;; string separating entries in the `separated' style
  (setq browse-kill-ring-separator "\n----------------------------------------------------------------")

  ;; temporarily highlight the inserted `kill-ring' entry
  (setq browse-kill-ring-highlight-inserted-item t)

  ;; face in which to highlight the `browse-kill-ring-separator'
  (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
  (setq browse-kill-ring-separator-face 'separator-face)

  ;; use `M-y' to invoke `browse-kill-ring'
  (browse-kill-ring-default-keybindings))

;; Set up tramp
(setq tramp-default-method "ssh")

;; Set up pastie
(autoload 'pastie-region "pastie" "\
Post the current region as a new paste at pastie.org.
Copies the URL into the kill ring." t nil)

(autoload 'pastie-buffer "pastie" "\
Post the current buffer as a new paste at pastie.org.
Copies the URL into the kill ring." t nil)

;; Set up gist
(autoload 'gist-region "gist" "\
Post the current region as a new paste at gist.github.com.
Copies the URL into the kill ring.
\(fn BEGIN END &optional PRIVATE)" t nil)

(autoload 'gist-buffer "gist" "\
Post the current buffer as a new paste at gist.github.com.
Copies the URL into the kill ring.
\(fn &optional PRIVATE)" t nil)

;; Other editing-related settings
(setq next-line-add-newlines nil
      find-file-use-truenames nil
      find-file-compare-truenames t
      make-backup-files nil)

(setq-default indent-tabs-mode nil)

;;; end ~/emacs/lisp/editing-config.el

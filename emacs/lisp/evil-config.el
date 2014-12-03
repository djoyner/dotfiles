(require 'djoyner-funcs)
(require 'evil)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-surround)
(require 'evil-visualstar)
(require 'linum)
(require 'misc-funcs)

;; Escape quits globally
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)

;; Normal state mappings
(define-key evil-normal-state-map "A" 'djoyner/electric-append-with-indent)
(define-key evil-normal-state-map "Y" "y$")
(define-key evil-normal-state-map "0" 'djoyner/smart-home)
(define-key evil-normal-state-map "$" 'djoyner/smart-end)
(define-key evil-normal-state-map (kbd "M-.") nil)

;; Visual state mappings

; DEL deletes selection
(define-key evil-visual-state-map (kbd "DEL") 'evil-delete)

; Overload shifts so that they don't lose the selection
(define-key evil-visual-state-map ">" 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map "<" 'djoyner/evil-shift-left-visual)
(define-key evil-visual-state-map (kbd "TAB") 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<backtab>") 'djoyner/evil-shift-left-visual)

;; Motion state mappings

; Move RET and SPC key bindings from the motion state map to the normal state map
; so that when modes define them, RET and SPC bindings are available directly
(move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(move-key evil-motion-state-map evil-normal-state-map " ")

; Move by display lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

; Stop trying to lookup man pages
(define-key evil-motion-state-map "K" nil)

;; Cursors
(setq evil-emacs-state-cursor '("red" box)
      evil-normal-state-cursor '("#f8f893" box)
      evil-insert-state-cursor '("#f8f893" bar)
      evil-replace-state-cursor '("#cc9393" box))

;; Leaders
(evil-leader/set-key
  "SPC" 'just-one-space
  "DEL" 'delete-trailing-whitespace
  "TAB" 'untabify
  "S-TAB" 'tabify
  "\\" 'evil-search-highlight-persist-remove-all
  "i" 'whitespace-mode
  "P" 'djoyner/evil-paste-clipboard-before
  "p" 'djoyner/evil-paste-clipboard-after
  "q" 'fill-paragraph
  "R" 'rename-file-and-buffer
  "y" "\"\"y")

(global-evil-leader-mode t)

;; Matching
(setq global-evil-matchit-mode t)
(define-key evil-normal-state-map "%" 'evilmi-jump-items)

;; Surround
(global-evil-surround-mode t)

;; Undo
(setq evil-want-fine-undo t)

;; Mode hacking
(add-hook 'evil-normal-state-entry-hook (lambda () (linum-mode 1)))
(add-hook 'evil-insert-state-entry-hook (lambda () (linum-mode 1)))

(evil-set-initial-state 'diff-mode 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'shell-mode 'emacs)

;; Override various key bindings in other modes (see also dired-config.el)
(evil-add-hjkl-bindings ibuffer-mode-map 'emacs
  "J" 'ibuffer-jump-to-buffer
  "K" 'ibuffer-mark-for-delete)

(evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
  "K" 'magit-discard-item
  "L" 'magit-key-mode-popup-logging)

(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk
  (kbd "C-b") 'scroll-down-command
  (kbd "C-f") 'scroll-up-command)

(evil-add-hjkl-bindings magit-log-mode-map 'emacs
  (kbd "C-b") 'scroll-down-command
  (kbd "C-f") 'scroll-up-command)

(evil-add-hjkl-bindings magit-commit-mode-map 'emacs
  (kbd "C-b") 'scroll-down-command
  (kbd "C-f") 'scroll-up-command)

;; Become evil
(evil-mode t)

(provide 'evil-config)

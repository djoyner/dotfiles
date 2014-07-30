(require 'evil)
(require 'evil-leader)
(require 'misc-funcs)
(require 'surround)

;; Escape quits globally
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; Normal state mappings

; Easy window navigation
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

; Y behaves as you'd expect
(define-key evil-normal-state-map "Y" 'djoyner/copy-to-end-of-line)

; Move RET and SPC kley bindings from the motion state map to the normal state map
; so that when modes define them, RET and SPC bindings are available directly
(move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(move-key evil-motion-state-map evil-normal-state-map " ")

;; Visual state mappings

; DEL deletes selection
(define-key evil-visual-state-map (kbd "DEL") 'evil-delete)

; Overload shifts so that they don't lose the selection
(define-key evil-visual-state-map (kbd ">") 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'djoyner/evil-shift-left-visual)
(define-key evil-visual-state-map [tab] 'djoyner/evil-shift-right-visual)
(define-key evil-visual-state-map [S-tab] 'djoyner/evil-shift-left-visual)

;; Motion state mappings

; Nuke the arrow keys
(define-key evil-motion-state-map [up] "")
(define-key evil-motion-state-map [down] "")
(define-key evil-motion-state-map [left] "")
(define-key evil-motion-state-map [right] "")

; Move by display lines
(define-key evil-motion-state-map "j" 'evil-next-visual-line)
(define-key evil-motion-state-map "k" 'evil-previous-visual-line)

; Stop trying to lookup man pages
(define-key evil-motion-state-map "K" nil)

;; Leaders
(evil-leader/set-key
  "SPC" 'just-one-space
  "DEL" 'delete-trailing-whitespace
  "TAB" 'untabify
  "S-TAB" 'tabify
  "\\" 'evil-ex-nohighlight
  "e" 'eval-last-sexp
  "i" 'whitespace-mode
  "n" 'make-frame-command
  "P" 'djoyner/evil-paste-clipboard-before
  "p" 'djoyner/evil-paste-clipboard-after
  "R" 'rename-file-and-buffer
  "t" 'djoyner/evil-set-tab-width
  "x" 'execute-extended-command
  "y" "\"*y")

;; Other mode mappings

;; Cursors
(setq evil-default-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
      evil-emacs-state-cursor '("red" box))

;; Other config
(setq evil-want-fine-undo t)

;; NB: evil-leader-mode must be enabled before evil-mode
(global-evil-leader-mode t)
(global-surround-mode t)
(evil-mode t)

;; Functions
(defun djoyner/evil-paste-clipboard-before ()
  (interactive)
  (evil-paste-before 1 ?*))

(defun djoyner/evil-paste-clipboard-after ()
  (interactive)
  (evil-paste-after 1 ?*))

(defun djoyner/evil-shift-left-visual ()
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-shift-right-visual ()
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun djoyner/evil-set-tab-width (value)
  (interactive "ntab-width: ")
  (set-variable 'tab-width value))

(provide 'evil-config)

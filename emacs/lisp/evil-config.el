(require 'evil)
(require 'evil-leader)
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
  "b" 'iswitchb-buffer
  "B" 'list-buffers
  "c" 'delete-window
  "e" 'djoyner/evil-edit
  "i" 'whitespace-mode
  "n" 'evil-window-new
  "o" 'delete-other-windows
  "P" 'djoyner/evil-paste-clipboard-before
  "p" 'djoyner/evil-paste-clipboard-after
  "r" 'evil-read
  "R" 'rename-file-and-buffer
  "s" 'djoyner/evil-edit-split
  "v" 'djoyner/evil-edit-vsplit
  "w" 'evil-write
  "x" 'execute-extended-command
  "y" "\"*y")

;; Other mode mappings

; Override j/k mappings for ibuffer mode
(eval-after-load 'ibuffer
    '(progn
       ;; use the standard ibuffer bindings as a base
       (message "Setting up ibuffer mappings")
       (set-keymap-parent
        (evil-get-auxiliary-keymap ibuffer-mode-map 'normal t)
        (assq-delete-all 'menu-bar (copy-keymap ibuffer-mode-map)))
       (evil-define-key 'normal ibuffer-mode-map "j" 'ibuffer-forward-line)
       (evil-define-key 'normal ibuffer-mode-map "k" 'ibuffer-backward-line)
       (evil-define-key 'normal ibuffer-mode-map "J" 'ibuffer-jump-to-buffer) ; "j"
     ))

;; Cursors
(setq evil-default-cursor '("white" box)
      evil-insert-state-cursor '("white" bar)
      evil-emacs-state-cursor '("red" box))

;; Other config
(evil-set-initial-state 'ibuffer-mode 'normal)

(setq evil-emacs-state-modes (delete 'ibuffer-mode evil-emacs-state-modes)
      evil-motion-state-modes (cons 'ibuffer-mode evil-motion-state-modes)
      evil-want-fine-undo t)

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

(defun djoyner/evil-edit (file)
  (interactive "F:edit ")
  (find-file file))

(defun djoyner/evil-edit-split (file)
  (interactive "F:split ")
  (let ((new-win (split-window (selected-window))))
    (find-file file)))

(defun djoyner/evil-edit-vsplit (file)
  (interactive "F:vsplit ")
  (let ((new-win (split-window (selected-window) nil t)))
    (find-file file)))

(provide 'evil-config)

;; Define inferior shells
(require 'defshell)

(setq defshell-reuse-buffer nil
      defshell-rename-buffer-uniquely t)

(defshell "/bin/bash" "bash")

;; Args passed to inferior shell, if the shell is bash
(setq explicit-bash-args '("--noediting" "--login" "-i"))

;; Translate ansi color sequences into text properties
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Smarter up/down bindings for shells -- scroll through command history if at command line, otherwise scroll through buffer
(defun shell-maybe-up (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-previous-input arg)
    (previous-line arg)
    ))

(defun shell-maybe-down (arg)
  (interactive "p")
  (if (comint-after-pmark-p)
      (comint-next-input arg)
    (next-line arg)
    ))

;; Clear shell contents
(defun shell-clear-region ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))

(define-key shell-mode-map [up] 'shell-maybe-up)
(define-key shell-mode-map [down] 'shell-maybe-down)

;; Prevent shell commands from being echoed
(when-ms-windows
 (defun my-comint-init ()
   (setq comint-process-echoes t))
 (add-hook 'comint-mode-hook 'my-comint-init))

(provide 'shell-config)

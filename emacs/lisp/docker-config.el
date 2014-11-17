(autoload 'dockerfile-mode "dockerfile-mode" "Major mode for editing Dockerfiles" t)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(provide 'docker-config)

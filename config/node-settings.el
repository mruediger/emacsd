(add-hook 'js-mode-hook
          (lambda() (local-set-key (kbd "<f6>")
                                   (lambda() (interactive) (shell-command (concat "node " (buffer-file-name)))))))

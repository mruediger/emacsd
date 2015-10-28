(require 'python)

(add-hook 'python-mode-hook
          (lambda() (local-set-key (kbd "<f6>")
                                   (lambda() (interactive) (shell-command (concat "nosetests -s " (buffer-file-name)))))))

(add-hook 'python-mode-hook
          (lambda() (local-set-key (kbd "<f7>")
                                   (lambda() (interactive) (shell-command (concat "python " (buffer-file-name)))))))

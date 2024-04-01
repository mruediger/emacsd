(require 'eglot)

(straight-use-package 'gotest)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(with-eval-after-load 'go-ts-mode
  (keymap-set go-ts-mode-map "C-c C-c" 'go-test-current-project)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq go-ts-mode-indent-offset 2)
  (setq go-test-verbose t)

  (add-hook 'go-ts-mode-hook (lambda ()
                               (setq tab-width 2))))

(provide 'module-go)

(require 'eglot)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(with-eval-after-load 'go-ts-mode
  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq go-ts-mode-indent-offset 2)
  (setq go-test-verbose t)

  (keymap-set go-ts-mode-map "C-c C-c" 'compile)
  (add-hook 'go-ts-mode-hook (lambda () (setq-local compile-command (concat "go test"))))
  (add-hook 'go-ts-mode-hook (lambda () (setq tab-width 2))))

(provide 'module-go)

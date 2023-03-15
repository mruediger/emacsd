(require 'module-lsp)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(with-eval-after-load 'go-ts-mode
  (keymap-set go-ts-mode-map "C-c C-c" 'go-test-current-project)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq  go-ts-mode-indent-offset 4)

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq standard-indent 2)
              (setq indent-tabs-mode nil)))

  (add-hook 'go-ts-mode-hook 'lsp-go-install-save-hooks)
  (add-hook 'go-ts-mode-hook 'flycheck-mode)
  (add-hook 'go-ts-mode-hook 'lsp-deferred))

(provide 'module-go)

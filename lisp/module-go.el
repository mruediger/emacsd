(require 'module-lsp)

;; switch to go-mode if go-ts-mode gets loaded
(add-hook 'go-ts-mode-hook 'go-mode)

(with-eval-after-load 'go-mode
  (keymap-set go-mode-map "C-c C-c" 'go-test-current-project)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq go-test-verbose t)


  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq standard-indent 2)
              (setq indent-tabs-mode nil)))



  (add-hook 'go-mode-hook 'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (add-hook 'go-mode-hook 'lsp-deferred))

(provide 'module-go)

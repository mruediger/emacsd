(require 'eglot)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))

(with-eval-after-load 'go-ts-mode
  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq go-ts-mode-indent-offset 2)
  (setq go-test-verbose t)

  (defun eglot-before-save () (interactive)
         (eglot-format-buffer)
         (call-interactively 'eglot-code-action-organize-imports))


  (keymap-set go-ts-mode-map "C-c C-c" 'compile)
  (add-hook 'go-ts-mode-hook '(lambda () (setq-local compile-command (concat "go test"))))
  (add-hook 'go-ts-mode-hook '(lambda () (setq tab-width 2)))
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'go-ts-mode-hook '(lambda () (add-hook 'before-save-hook 'eglot-before-save nil t))))



(provide 'module-go)

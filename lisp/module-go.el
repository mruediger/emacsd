(require 'eglot)

;; switch to go-mode if go-ts-mode gets loaded
(add-hook 'go-ts-mode-hook 'go-mode)

(with-eval-after-load 'go-mode
  (keymap-set go-mode-map "C-c C-c" 'go-test-current-project)

  ;; CamelCase aware editing operations
  (subword-mode +1)

  (setq go-test-verbose t)

  (add-hook 'go-mode-hook
            (lambda ()
              (setq tab-width 2)
              (setq standard-indent 2)
              (setq indent-tabs-mode nil)))

  (add-hook 'go-mode-hook
            (lamba ()
                   (add-hook 'before-save-hook #'eglot-format-buffer -10 t)))

  (add-hook 'go-mode-hook
            (lamba ()
                   (add-hook 'before-save-hook #'eglot-code-action-organize-imports -10 t)))

  (add-hook 'go-mode-hook 'eglot-ensure))

(provide 'module-go)

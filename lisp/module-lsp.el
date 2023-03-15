(require 'lsp-mode)
(require 'lsp-ui)

;; Terraform (https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls)
(setq lsp-disabled-clients '(tfls)
      lsp-terraform-ls-enable-show-reference t
      lsp-semantic-tokens-enable t
      lsp-semantic-tokens-honor-refresh-requests t
      lsp-enable-links t)

(setq lsp-enable-snippet nil)

(setq lsp-ui-sideline-enable t
      lsp-ui-doc-enable t
      lsp-ui-peek-enable t
      lsp-ui-peek-always-show t)

(add-hook 'lsp-mode-hook 'lsp-enable-which-key-integration)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(provide 'module-lsp)

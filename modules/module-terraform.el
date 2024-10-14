(use-package terraform-mode :straight t
  :mode ("\\.tf" . terraform-mode)
  :hook
  ;; workarround for https://github.com/hashicorp/terraform-ls/issues/1067
  (terraform-mode . (lambda () (setq-local create-lockfiles nil)))
  (terraform-mode . eglot-ensure)
  (terraform-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(provide 'module-terraform)

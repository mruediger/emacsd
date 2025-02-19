(use-package yaml-ts-mode
    :mode "\\.ya?ml\\'")

(use-package highlight-indent-guides :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(provide 'module-yaml)

;;; module-yaml.el -*- lexical-binding: t; -*-

(use-package yaml-ts-mode
  :bind (:map yaml-ts-mode-map ("C-c C-c" . compile))
  :mode "\\.ya?ml\\'")

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(provide 'module-yaml)

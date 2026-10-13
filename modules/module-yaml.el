;;; module-yaml.el -*- lexical-binding: t; -*-

(use-package yaml-ts-mode
  :bind (:map yaml-ts-mode-map ("C-c C-c" . compile))
  :mode "\\.ya?ml\\'")

(use-package highlight-indent-guides
  :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(use-package helm-ts-mode
  :straight (:type git :host github :repo "mosjek/helm-ts-mode"))

(provide 'module-yaml)

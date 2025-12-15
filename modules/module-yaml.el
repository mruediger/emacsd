(use-package yaml-ts-mode
  :bind (:map yaml-ts-mode-map ("C-c C-c" . compile))
  :mode "\\.ya?ml\\'")

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(use-package helm-ts-mode
  :straight (:type git :host github :repo "mosjek/helm-ts-mode"))

(define-hostmode poly-yaml-ts-hostmode :mode 'yaml-ts-mode)
(define-innermode poly-go-ts-template-innermode :mode 'go-ts-mode
  :head-matcher "{{"
  :tail-matcher "}}"
  :head-mode 'body
  :tail-mode 'body)

(define-polymode helm-template-mode
  :hostmode 'poly-yaml-ts-hostmode
  :innermodes '(poly-go-ts-template-innermode))

(provide 'module-yaml)

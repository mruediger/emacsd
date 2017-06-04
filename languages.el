(use-package lua-mode
  :defer t
  :ensure t)

(use-package dockerfile-mode
  :defer t
  :ensure t)

(use-package hcl-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.tf$" . hcl-mode))

(use-package markdown-mode
  :defer t
  :ensure t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(use-package yaml-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))

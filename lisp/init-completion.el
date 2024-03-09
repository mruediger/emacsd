(use-package yasnippet :straight t
  :init
  (use-package yasnippet-snippets :straight t))

(use-package company :straight t
  :hook (eglot-managed-mode . company-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)))

;; minimalistic vertical completion UI
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("TAB" . minibuffer-complete)
        ("M-TAB" . vertico-insert)))

;; margin annotation for the minibuffer
(use-package marginalia
  :straight t
  :init
  (marginalia-mode))

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode))

(provide 'init-completion)

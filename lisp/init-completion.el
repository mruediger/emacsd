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

;; completion during buffer selection, yanking, ...
(use-package consult
  :straight t
  :bind ("M-g l" . consult-line)
  :hook (completion-list-mode . consult-preview-at-point-mode))

(provide 'init-completion)

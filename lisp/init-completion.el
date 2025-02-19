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
  :bind
  ("M-g l" . consult-line)
  ("C-b b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ("M-g g" . consult-goto-line)             ;; orig. goto-line
  ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
  ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
  ("M-g m" . consult-mark)
  ("M-g k" . consult-global-mark)
  ("C-x C-g" . consult-ripgrep)

  :hook (completion-list-mode . consult-preview-at-point-mode))

(provide 'init-completion)

(use-package yasnippet :straight t
  :init
  (use-package yasnippet-snippets :straight t))

(use-package corfu :straight t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(substring orderless basic)))

;;completion-category-defaults nil
;;completion-category-overrides '((file (styles partial-completion)))))

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

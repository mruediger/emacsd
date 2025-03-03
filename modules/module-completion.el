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

(use-package yasnippet :straight t
  :init
  (yas-global-mode)
  (use-package yasnippet-snippets :straight t))

(use-package hippie-expand
  :bind ("M-/" . hippie-expand))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  :hook
  (nix-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure))

;; used by eglot for showing the drop down
(use-package company :straight t
  :hook (eglot-managed-mode . company-mode))

;; used by eglot for highlighting errors
(use-package flymake
  :hook (eglot-managed-mode . flymake-mode))

;; used by eglot for defining a lsp workspace/set of files
(use-package project
  :config
  (defun mr/project-try-rust (dir)
    "Find root directory based on Cargo.toml"
    (let* ((module-root (locate-dominating-file dir "Cargo.toml"))
           (is-vc-root (file-directory-p (expand-file-name ".git" module-root))))
      (when (and module-root (not is-vc-root))
        (cons 'transient module-root))))
  (setq project-find-functions '(mr/project-try-rust project-try-vc)))

(provide 'module-completion)

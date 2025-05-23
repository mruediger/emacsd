(use-package prog-mode
  :bind (:map prog-mode-map ("C-c C-c" . compile)))

(use-package compile
  :config
  (setq compilation-read-command nil)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  ;; Save all buffers on M-x `compile'
  (compilation-ask-about-save nil))

;; The unified debugger
(use-package gud
  :hook (gud-mode . gud-tooltip-mode)
  :custom
  (gud-highlight-current-line t))

;; Show trailing whitespaces
(use-package whitespace
  :hook ((prog-mode markdown-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face trailing)))

(use-package python
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  :hook
  (python-mode . (lambda ()
                   (setq-local compile-command (concat "python " buffer-file-name))))
  :bind (:map python-mode-map
              ("C-c C-c" . compile)))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :config
  (setq compile-command "cargo test -- --nocapture"))

(use-package nix-mode
  :config
  (defun sudo-compile () (interactive)
         (let ((default-directory (concat "/sudo::" (expand-file-name (project-root (project-current t))))))
               (call-interactively #'compile)))
  (setq compile-command "nixos-rebuild switch --flake '.#'")
  :bind (:map nix-mode-map ("C-c C-c" . sudo-compile)))

(use-package typst-ts-mode)

(use-package jsonnet-mode)

(use-package eglot-java
  :hook (java . eglot-java-mode))

(provide 'module-dev)

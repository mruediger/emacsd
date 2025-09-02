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

;; add color output to compilations
(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

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
  :hook
  (rust-ts-mode . (lambda () (setq-local compile-command "cargo test -- --nocapture"))))

(use-package nix-mode
  :config
  (defvar use-sudo-compile nil)
  (add-to-list 'safe-local-variable-values
             '(compile-command . "nixos-rebuild switch --flake '.#'"))
  (defun nix-compile () (interactive)
         (let* ((base-directory (expand-file-name (project-root (project-current t))))
                (default-directory (if use-sudo-compile (concat "/sudo::" base-directory) base-directory)))
           (call-interactively #'compile)))
  :bind (:map nix-mode-map ("C-c C-c" . nix-compile)))

(use-package typst-ts-mode)

(use-package jsonnet-mode
  :hook
  (jsonnet-mode . eglot-ensure))

(use-package eglot-java
  :hook (java . eglot-java-mode))

(use-package dockerfile-ts-mode
  :mode "Dockerfile")
(provide 'module-dev)

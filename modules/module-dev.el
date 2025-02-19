(use-package compile
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
  :ensure nil
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
  (rust-ts-mode . (lambda ()
                   (setq-local compile-command "cargo test -- --nocapture")))
  :bind (:map rust-ts-mode-map
              ("C-c C-c" . compile)))

(use-package nix-mode :straight t
  :config
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/src#'")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update)))


(provide 'module-dev)

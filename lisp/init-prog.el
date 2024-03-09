;; eglot, project & company


(use-package eglot
  :config
  (defun my-eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))

  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (add-hook 'before-save-hook #'my-eglot-organize-imports nil t)
                                       (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  :hook
  (nix-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure))

(use-package project
  :config
  (defun mr/project-try-rust (dir)
    "Find root directory based on Cargo.toml"
    (let* ((module-root (locate-dominating-file dir "Cargo.toml"))
           (is-vc-root (file-directory-p (expand-file-name ".git" module-root))))
      (when (and module-root (not is-vc-root))
        (cons 'transient module-root))))
  (setq project-find-functions '(mr/project-try-rust project-try-vc)))

(use-package compile
  :config
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t)
  (setq compilation-read-command nil))

(provide 'init-prog)

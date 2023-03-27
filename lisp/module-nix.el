(add-hook 'nix-mode-hook 'eglot)

(with-eval-after-load 'nix-mode
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/nixos-config#'")))


  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

  (keymap-set nix-mode-map "C-c C-c" 'nix-update)
  (add-hook 'nix-mode-hook 'eglot-format-buffer-on-save)
  (add-hook 'nix-mode-hook 'eglot-ensure))

(provide 'module-nix)

(require 'terraform-mode)
(require 'eglot)

(add-to-list 'eglot-server-programs
   		'(terraform-mode . ("terraform-ls" "serve")))

(add-hook 'terraform-mode-hook 'eglot-ensure)
(add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)

;; workarround for https://github.com/hashicorp/terraform-ls/issues/1067
(add-hook 'terraform-mode-hook (lambda () (setq-local create-lockfiles nil)))

(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))




(provide 'module-terraform)

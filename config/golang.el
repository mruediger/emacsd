(use-package go-mode :ensure t)
(use-package golint :ensure t)

(setq gofmt-command "goimports")

(defun gotest ()
  (interactive)
  (shell-command "go test"))

(add-hook 'go-mode-hook
          (lambda() (local-set-key (kbd "<f6>") 'gotest)))

(add-hook 'before-save-hook 'gofmt-before-save)

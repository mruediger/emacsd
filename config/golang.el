(require 'go-mode-load)

(setq gofmt-command "goimports")

(defun gotest ()
  (interactive)
  (shell-command "go test"))

(add-hook 'go-mode-hook
          (lambda() (local-set-key (kbd "<f6>") 'gotest)))

(add-hook 'go-mode-hook
          (lambda() (local-set-key (kbd "C-c C-f") 'gofmt)))

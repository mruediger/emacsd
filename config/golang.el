(require 'go-mode-load)

;(eval-after-load "go-mode"
;  '(define-key global-map (kbd "<f6>")
;     (lambda() (interactive) (shell-command "go test"))))

(defun gotest ()
  (interactive)
  (shell-command "go test"))

(add-hook 'go-mode-hook
          (lambda() (local-set-key (kbd "<f6>") 'gotest)))

(add-hook 'go-mode-hook
          (lambda() (local-set-key (kbd "C-c C-f") 'gofmt)))

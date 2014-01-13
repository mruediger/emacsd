(require 'go-mode-load)

(eval-after-load "go-mode"
  '(define-key global-map (kbd "<f6>")
     (lambda() (interactive) (shell-command "go test"))))

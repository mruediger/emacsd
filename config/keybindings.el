;; KEYBOARD QUIT
(define-key global-map (kbd "C-g") 'nil)
(define-key global-map (kbd "C-q") 'keyboard-quit)
(define-key minibuffer-local-map (kbd "C-q") 'abort-recursive-edit)


(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-z") 'undo)

(define-key global-map (kbd "C-b") nil)
(define-key global-map (kbd "C-x C-b") nil)
(define-key global-map (kbd "C-x b") nil)
(define-key global-map (kbd "C-b C-p") 'switch-to-prev-buffer)
(define-key global-map (kbd "C-b b") 'switch-to-buffer)
(define-key global-map (kbd "C-b C-b") 'ibuffer)

(define-key global-map (kbd "C-b C-s") (lambda() (interactive) (switch-to-buffer "*scratch*")))

(define-key global-map (kbd "C-c y") 'clipboard-yank)

(define-key global-map (kbd "C-s") 'isearch-forward)
(define-key global-map (kbd "C-r") 'isearch-backward)
(define-key global-map (kbd "C-M-s") 'isearch-forward)
(define-key global-map (kbd "C-M-r") 'isearch-backward)


(define-key global-map (kbd "C-o") 'other-window)

(define-key global-map (kbd "C-x C-k") 'kill-buffer)


(define-key global-map (kbd "S-DEL") 'kill-whole-line)

(define-key global-map (kbd "C-c C-c") 'comment-or-uncomment-region)


;; GIT
(define-key global-map (kbd "C-g s") 'magit-status)

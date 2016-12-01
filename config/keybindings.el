;; BASICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq echo-keystrokes 0.02)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-z") 'undo)

(define-key global-map (kbd "<insert>") 'nil)

(define-key global-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key global-map (kbd "M-x") 'execute-extended-command)

;; KEYBOARD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-g") 'nil)
(define-key global-map (kbd "C-q") 'keyboard-quit)

(define-key global-map (kbd "<escape>") 'keyboard-quit)

(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-escape-quit)

(define-key minibuffer-local-map (kbd "C-q") 'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "C-q") 'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "C-q") 'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "C-q") 'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "C-q") 'keyboard-escape-quit)


;; SEARCH AND REPLACE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-s") 'isearch-forward)
(define-key global-map (kbd "C-M-s") 'isearch-backward)
(define-key global-map (kbd "C-r") 'query-replace)




(define-key global-map (kbd "C-o") 'other-window)




(define-key global-map (kbd "S-DEL") 'kill-whole-line)

(define-key global-map (kbd "C-c C-c") 'comment-or-uncomment-region)


;; BUFFERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;j;;;;;;;;
(define-key global-map (kbd "C-b") nil)
(define-key global-map (kbd "C-x C-b") nil)
(define-key global-map (kbd "C-x b") nil)

(define-key global-map (kbd "C-b C-p") 'switch-to-prev-buffer)
(define-key global-map (kbd "C-b b") 'switch-to-buffer)
(define-key global-map (kbd "C-b C-b") 'ibuffer)
(define-key global-map (kbd "C-b C-k") 'kill-buffer)
(define-key global-map (kbd "C-b C-s") (lambda() (interactive) (switch-to-buffer "*scratch*")))

;; GIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key global-map (kbd "C-g s") 'magit-status)

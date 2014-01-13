(when window-system
  (set-face-attribute 'default nil :font "Droid Sans Mono-9")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'solarized-light t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-distinct-fringe-background t))
  ;;(load-theme 'zenburn t))

;; (set-frame-parameter (selected-frame) 'alpha '(95 80))
;; (add-to-list 'default-frame-alist '(alpha 95 80))

;; (load-theme 'solarized-light t))

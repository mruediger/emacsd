(when window-system
  (set-face-attribute 'default nil :font "Droid Sans Mono-9")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  (load-theme 'solarized-light t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-distinct-fringe-background t))
  ;;(load-theme 'zenburn t))

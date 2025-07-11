(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; cleanup user interface - remove unneded fluff
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(menu-bar-mode -1)     ; Disable menu bar
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-time-mode t)


(set-face-attribute 'default nil :font "Iosevka" :height 120)
(set-fontset-font t nil (font-spec :size 20 :name "Iosevka Nerd Font"))


(setq frame-title-format "emacs: %b")

;;split windows proportionally
(setq window-combination-resize 't)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(provide 'init-ui)

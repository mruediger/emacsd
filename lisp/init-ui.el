;; User Interface
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

(setq solarized-scale-org-headlines nil)
(setq solarized-use-variable-pitch nil)
(load-theme 'solarized-light t)
(let ((line (face-attribute 'mode-line :underline)))
  (set-face-attribute 'mode-line          nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :overline   line)
  (set-face-attribute 'mode-line-inactive nil :underline  line)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))

(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(moody-replace-eldoc-minibuffer-message-function)




(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "FontAwesome"))

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 130)

(setq frame-title-format "emacs: %b")

;;split windows proportionally
(setq window-combination-resize 't)

;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)

;; Show keybindings with which-key
(which-key-mode)
(setq which-key-idle-delay 0.5)

(provide 'init-ui)

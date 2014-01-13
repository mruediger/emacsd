; WINDOW SETTINGS
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

; TURN OFF BELL
(setq ring-bell-function 'ignore)

; BASIC MODES
(column-number-mode t)
(line-number-mode t)

;; (electric-pair-mode t)
(global-linum-mode -1)

; BASIC HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)


; BASIC EDITING
(setq-default tab-width 2)
(setq-default indent-tabs-mode 'nil)


; BACKUP SETTINGS
(setq backup-inhibited t)
(setq auto-save-default nil)

(setq mouse-yank-at-point t)

(require 'server)
(unless (server-running-p)
    (server-start))

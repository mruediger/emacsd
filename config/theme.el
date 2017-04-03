;(use-package powerline :ensure t)
;(use-package darcula-theme :ensure t)
;(use-package soft-morning-theme :ensure t)
;(use-package darktooth-theme :ensure t)
;(use-package zenburn-theme :ensure t)

(add-to-list 'default-frame-alist '(font . "PragmataPro-10"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")

;;(set-default-font "PragmataPro-9")

(powerline-default-theme)


;; (defun light-theme ()
;;   (interactive)
;;   (load-theme 'solarized-light t))

;; (defun dark-theme ()
;;   (interactive)
;;   (load-theme 'darcula t))

;(load-theme 'solarized-dark t)
;(load-theme 'zenburn t)
;(set-frame-parameter nil 'background-mode 'light)
(load-theme 'solarized t)


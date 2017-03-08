(use-package powerline :ensure t)
(use-package darcula-theme :ensure t)
(use-package soft-morning-theme :ensure t)
(use-package darktooth-theme :ensure t)

(add-to-list 'default-frame-alist '(font . "PragmataPro-10"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(powerline-default-theme)

(defun dark-theme ()
  (interactive)
  (load-theme 'darcula t))

(defun light-theme ()
  (interactive)
  (load-theme 'soft-morning t))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; BASICS
(load-file "~/.emacs.d/config/base.el")
(load-file "~/.emacs.d/config/keybindings.el")
(load-file "~/.emacs.d/config/packages.el")
(load-file "~/.emacs.d/config/theme.el")
(load-file "~/.emacs.d/config/git.el")
(load-file "~/.emacs.d/config/projectile-settings.el")
(load-file "~/.emacs.d/config/stuff.el")

;; LANGUAGES
(load-file "~/.emacs.d/config/golang.el")
(load-file "~/.emacs.d/config/rust.el")
(load-file "~/.emacs.d/config/python-settings.el")
(load-file "~/.emacs.d/config/node-settings.el")
(load-file "~/.emacs.d/config/scala.el")
(load-file "~/.emacs.d/languages.el")

;;
;; IDO settings
;;
(use-package flx-ido
  :ensure t
  :demand
  :init
  (setq
   ido-enable-flex-matching t
   ido-everywhere t)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (flx-ido-mode t))

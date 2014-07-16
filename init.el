(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; BASICS
(load-file "~/.emacs.d/config/base.el")
(load-file "~/.emacs.d/config/keybindings.el")
(load-file "~/.emacs.d/config/theme.el")

;; LANGUAGES
(load-file "~/.emacs.d/config/golang.el")
(load-file "~/.emacs.d/config/markdown.el")
(load-file "~/.emacs.d/config/python-settings.el")

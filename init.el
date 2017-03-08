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
(load-file "~/.emacs.d/config/markdown.el")
(load-file "~/.emacs.d/config/python-settings.el")
(load-file "~/.emacs.d/config/node-settings.el")
(load-file "~/.emacs.d/config/yaml.el")
(load-file "~/.emacs.d/config/hcl.el")
(load-file "~/.emacs.d/config/scala.el")

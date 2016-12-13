;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; BASICS
(load-file "~/.emacs.d/config/base.el")
(load-file "~/.emacs.d/config/keybindings.el")
(load-file "~/.emacs.d/config/theme.el")
(load-file "~/.emacs.d/config/melpa.el")
(load-file "~/.emacs.d/config/git.el")
(load-file "~/.emacs.d/config/completion.el")

;; LANGUAGES
(load-file "~/.emacs.d/config/golang.el")
(load-file "~/.emacs.d/config/rust.el")
(load-file "~/.emacs.d/config/markdown.el")
(load-file "~/.emacs.d/config/python-settings.el")
(load-file "~/.emacs.d/config/node-settings.el")
(load-file "~/.emacs.d/config/yaml.el")
(load-file "~/.emacs.d/config/hcl.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (helm powerline xkcd magit-gh-pulls magit))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

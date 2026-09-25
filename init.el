(defconst my-local-dir (expand-file-name "local/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(require 'init-bootstrap-straight)
(require 'init-gc)
(require 'init-ui)
(require 'init-editor)
(require 'init-keybindings)
(require 'init-email)

(require 'module-completion)
(require 'module-org)
(require 'module-git)
(require 'module-dev)
(require 'module-ai)
(require 'module-yaml)
(require 'module-go)
(require 'module-terraform)
(require 'module-dired)
(require 'module-dart)
(require 'module-jira)
(require 'module-misc)

;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)

(use-package envrc
  :straight t
  :hook (after-init . envrc-global-mode))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(use-package sudo-edit
  :straight t)

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-buffer)))

(use-package tramp
  :config
  (add-to-list 'tramp-methods
	       '("gcssh"
		 (tramp-login-program "gcloud")
		 (tramp-login-args (("compute ssh --tunnel-through-iap") ("%h") ("--ssh-flag='-l %u'")))
		 (tramp-async-args (("-q")))
		 (tramp-remote-shell "/bin/bash")
		 (tramp-remote-shell-args ("-c"))
		 (tramp-default-port 22))))

(use-package ledger-mode
  :straight t)

(use-package beancount
  :straight (beancount-mode :type git :host github :repo "beancount/beancount-mode"))

(use-package markdown-mode
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode))

(use-package copy-as-format
  :straight t)

(use-package csv-mode
  :straight t
  :config
  (setq csv-separators '("," ";")))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package rego-mode
  :straight t)

(defun insert-quotes (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :init
  (global-treesit-fold-mode)
  :bind
  ("C-x f c" . treesit-fold-close)
  ("C-x f o" . treesit-fold-open)
  ("C-x f a c" . treesit-fold-close-all)
  ("C-x f a o" . treesit-fold-open-all))

(use-package pass
  :straight t)

;; XKCD
(use-package xkcd
  :straight t
  :config
  (defun xkcd-emacs () (interactive) (xkcd-get 378)))

(use-package pdf-tools
  :straight t
  :config
  (pdf-loader-install))

(use-package nov
  :straight t
  :mode ("\\.epub\\'" . nov-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

(defconst my-local-dir (expand-file-name "local/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

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

;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)


;; direnv
(use-package direnv
  :init
  (direnv-mode))

(use-package flycheck
  :hook (emacs-lisp . flycheck-mode))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :after ispell
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))


(use-package ag
  :config
  (defun mr/ag-project-with-thing-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (ag-project thing))))

;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(use-package sudo-edit)

(use-package elisp
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

(use-package ledger-mode)

(use-package markdown
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode))

(use-package copy-as-format)

(use-package use-package-chords
  :init (key-chord-mode 1))

(use-package window
  :chords ((" 0" . delete-window)
           (" 1" . delete-other-windows)
           (" 2" . split-window-below)
           (" 3" . split-window-right)))

(use-package csv-mode)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package rego-mode)

(defun insert-quotes (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



(use-package pdf-tools)

(use-package treesit-fold
  :vc (:url "git@github.com:emacs-tree-sitter/treesit-fold.git")
  :init
  (global-treesit-fold-mode)
  :bind
  ("C-x f c" . treesit-fold-close)
  ("C-x f o" . treesit-fold-open)
  ("C-x f a c" . treesit-fold-close-all)
  ("C-x f a o" . treesit-fold-open-all))

(use-package pass)

;; XKCD
(use-package xkcd
  :config
  (defun xkcd-emacs () (interactive) (xkcd-get 378)))

(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(defconst my-local-dir (expand-file-name "local/" user-emacs-directory))
(defconst my-lisp-dir (expand-file-name "lisp/" user-emacs-directory))
(defconst my-cache-dir (expand-file-name "cache/" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(add-to-list 'load-path (expand-file-name "core/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

(require 'init-bootstrap-straight)
(require 'init-gc)
(require 'init-ui)

(require 'init-editor)
(require 'init-keybindings)
(require 'init-email)

(require 'init-files)

(require 'init-completion)

(require 'module-completion)
(require 'module-org)
(require 'module-git)
(require 'module-dev)
(require 'module-ai)

(require 'module-go)
(require 'module-terraform)




;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)


;; direnv
(use-package direnv :straight t
  :init
  (direnv-mode))




(use-package flycheck :straight t
  :hook (emacs-lisp . flycheck-mode))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :after ispell
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))


(use-package ag :straight t
  :config
  (defun mr/ag-project-with-thing-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (ag-project thing))))

;;
;; Languages
;;
(use-package nix-mode :straight t
  :config
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/src#'")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update)))


;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

(use-package yaml-ts-mode
    :mode "\\.ya?ml\\'")

(use-package highlight-indent-guides :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-ts-mode . highlight-indent-guides-mode))

(use-package jsonnet-mode :straight t)

(use-package python
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  :hook
  (python-mode . (lambda ()
                   (setq-local compile-command (concat "python " buffer-file-name))))
  :bind (:map python-mode-map
              ("C-c C-c" . compile)))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :hook
  (rust-ts-mode . (lambda ()
                   (setq-local compile-command "cargo test -- --nocapture")))
  :bind (:map rust-ts-mode-map
              ("C-c C-c" . compile)))

(use-package sudo-edit :straight t)

(use-package elisp
  :bind ("C-c C-c" . eval-buffer))

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

(use-package ledger-mode :straight t)

(use-package markdown
  :hook
  (markdown-mode . visual-line-mode)
  (markdown-mode . flyspell-mode))

(use-package copy-as-format :straight t)

(use-package use-package-chords :straight t
  :init (key-chord-mode 1))

(use-package "window"
  :chords ((" 0" . delete-window)
           (" 1" . delete-other-windows)
           (" 2" . split-window-below)
           (" 3" . split-window-right)))

(use-package csv-mode :straight t)

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package rego-mode :straight t)


(defun insert-quotes (&optional arg)
  (interactive "P")
  (insert-pair arg ?\" ?\"))


(setq compilation-read-command nil)
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



(use-package pdf-tools :straight t)

(use-package treesit-fold
  :straight (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :init
  (global-treesit-fold-mode)
  :bind
  ("C-x f c" . treesit-fold-close)
  ("C-x f o" . treesit-fold-open)
  ("C-x f a c" . treesit-fold-close-all)
  ("C-x f a o" . treesit-fold-open-all))

(defconst my-local-dir (expand-file-name "local/" user-emacs-directory))
(defconst my-lisp-dir (expand-file-name "lisp/" user-emacs-directory))

(add-to-list 'load-path my-lisp-dir)

(require 'init-bootstrap)
(require 'init-gc)
(require 'init-ui)

(require 'init-editor)
(require 'init-keybindings)
(require 'init-email)

;;
;; Setup UI
;;


;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)


;; direnv
(use-package direnv :straight t
  :init
  (direnv-mode))



;;
;; Magit
;;
(use-package magit :straight t
  :bind
  (("C-x g s"   . magit-status)
   ("C-x g p"   . magit-push-current-to-pushremote)
   ("C-x g l b" . magit-log-buffer-file)
   ("C-x g l c" . magit-log-current)
   ("C-x g l a" . magit-log-all)))

(use-package forge :straight t
  :after magit)

;;(use-package code-review
;;  :straight (:host github :repo "phelrine/code-review" :branch "fix/closql-update")
;;  :after magit
;;  :custom
;;  (code-review-download-dir (concat minemacs-cache-dir "code-review/"))
;;  (code-review-db-database-file (concat minemacs-local-dir "code-review/database.sqlite"))
;;  (code-review-log-file (concat minemacs-local-dir "code-review/code-review-error.log"))
;;  (code-review-auth-login-marker 'forge) ; use the same credentials as forge in ~/.authinfo.gpg
;;  :init
;;  (with-eval-after-load 'magit
;;    (transient-append-suffix 'magit-merge "i"
;;      '("y" "Review pull-request" code-review-forge-pr-at-point)))
;;  (with-eval-after-load 'forge
;;    (transient-append-suffix 'forge-dispatch "c u"
;;      '("c r" "review pull-request" code-review-forge-pr-at-point))))

;;
;; Development
;;
(use-package eglot
  :config
  (defun my-eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))

  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (add-hook 'before-save-hook #'my-eglot-organize-imports nil t)
                                       (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  :hook
  (nix-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (yaml-ts-mode . eglot-ensure))

(use-package flycheck :straight t
  :hook (emacs-lisp . flycheck-mode))

(use-package corfu :straight t
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode))

(use-package yasnippet :straight t
  :init
  (use-package yasnippet-snippets :straight t))

(use-package flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :after ispell
  :hook ((text-mode . turn-on-flyspell)
         (prog-mode . flyspell-prog-mode)))

(use-package project
  :config
  (defun mr/project-try-rust (dir)
    "Find root directory based on Cargo.toml"
    (let* ((module-root (locate-dominating-file dir "Cargo.toml"))
           (is-vc-root (file-directory-p (expand-file-name ".git" module-root))))
      (when (and module-root (not is-vc-root))
        (cons 'transient module-root))))
  (setq project-find-functions '(mr/project-try-rust project-try-vc)))

(use-package ag :straight t
  :config
  (defun mr/ag-project-with-thing-at-point ()
    (interactive)
    (let ((thing (thing-at-point 'symbol)))
      (ag-project thing))))

(use-package compile
  :config
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t)
  (setq compilation-read-command nil))

;;
;; Languages
;;
(use-package nix-mode :straight t
  :config
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/src#'")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update)))

(use-package terraform-mode :straight t
  :hook
  ;; workarround for https://github.com/hashicorp/terraform-ls/issues/1067
  (terraform-mode . (lambda () (setq-local create-lockfiles nil))))

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

(use-package go-ts-mode
  :config
  (setq go-ts-mode-indent-offset 2)
  :hook
  (go-ts-mode . (lambda () (setq tab-width 2))))

(use-package gotest :straight t
  :after go-ts-mode
  :config
  (setq go-test-verbose t)
  :bind (:map go-ts-mode-map
              ("C-c C-c" . go-test-current-project)))

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

(use-package chatgpt-shell :straight t
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "dev/openai-key")))))

(use-package dall-e-shell :straight t)

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

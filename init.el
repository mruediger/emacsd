(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-ui)
(require 'init-editor)
(require 'init-global-keybindings)
(require 'init-email)

(require 'module-go)
(require 'module-magit)
(require 'module-nix)
(require 'module-terraform)
(require 'module-org)

;;
;; PACKAGE MANAGEMENT
;;
(setq use-package-always-defer t)
(require 'use-package)

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;;
;; Development
;;

;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;; flycheck
(use-package flycheck
  :hook (emacs-lisp . flycheck-mode))

(use-package corfu
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

(use-package yasnippet)

;; Languages
(use-package yaml-mode
  :config
  (setq yas-indent-line 'fixed)
  :mode ("\\.sls\\'" . yaml-mode))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-mode . highlight-indent-guides-mode))

(use-package jsonnet-mode)

(use-package python
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  (setq-local compile-command (concat "python " buffer-file-name))
  :bind (:map python-mode-map
              ("C-c C-c" . recompile)))

;;SUDO-EDIT
(use-package sudo-edit)

(use-package tramp
  :config
  (add-to-list 'tramp-methods
			   '("gcssh"
				 (tramp-login-program "gcloud")
				 (tramp-login-args (("compute ssh") ("%h") ("--ssh-flag='-l %u'")))
				 (tramp-async-args (("-q")))
				 (tramp-remote-shell "/bin/bash")
				 (tramp-remote-shell-args ("-c"))
				 (tramp-default-port 22))))

(use-package ledger-mode)

(use-package notmuch)

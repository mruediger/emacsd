(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-ui)
(require 'init-editor)
(require 'init-global-keybindings)
(require 'init-email)

(require 'module-go)
(require 'module-terraform)

;;
;; PACKAGE MANAGEMENT
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq use-package-always-defer t)
(require 'use-package)

;; direnv
(use-package direnv :straight t
  :init
  (direnv-mode))


;;
;; Org Mode
;;
(use-package org :straight t
  :config
  (use-package org-trello :straight (:build (:not compile)))

  (add-to-list 'org-modules 'org-tempo t)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell      . t)
			         (emacs-lisp . t)
			         (python     . t)
                                 (R          . t)
                                 (gnuplot    . t)
                                 (js         . t)))

  (setq org-confirm-babel-evaluate nil)
  ;; Pretty code blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (setq org-duration-format (quote h:mm))

  (setq org-directory "~/org"
        org-refile-use-outline-path 'file)
  (setq org-agenda-files '("~/org")
        org-agenda-window-setup (quote current-window))
  (setq org-capture-templates
        '(("t" "task" entry (file "inbox.org") "* TODO %?\n")
          ("n" "note" entry (file "inbox.org") "* TODO %?\n %a")))
  (setq org-export-with-toc nil)
  (setq org-agenda-custom-commands
        '(("c" "Agenda and TODO" ((agenda "" ((org-agenda-span 9)
                                              (org-agenda-start-day "-2d")))
                                  (alltodo "" ((org-agenda-todo-ignore-deadlines (quote all))
                                               (org-agenda-todo-ignore-scheduled (quote all))))))))

  (defun org-agenda-show-agenda-and-todo () (interactive) (org-agenda nil "c"))
  (defun org-open-inbox () (interactive) (find-file "~/org/inbox.org"))
  (defun org-open-today () (interactive) (find-file (concat "~/org/today-" (format-time-string "%Y-%m-%d") ".org")))
  :bind
  ("C-x o a" . org-agenda-show-agenda-and-todo)
  ("C-x o t" . org-todo-list)
  ("C-x o c" . org-capture)
  ("C-x o i" . org-open-inbox)
  ("C-x o n" . org-open-today)
  :hook
  (org-mode . visual-line-mode))

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

;;
;; Development
;;
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (add-hook 'before-save-hook #'eglot-format-buffer nil t)
                                       (add-hook 'before-save-hook #'eglot-organize-imports-on-save nil t)))
  :hook
  (nix-mode . eglot-ensure))

;;
;; Nix
;;
(use-package nix-mode :straight t
  :config
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/nixos-config#'")))
  :bind ("C-c C-c" . nix-update)
  :hook ((nix-mode . eglot-format-buffer-on-save)
         (nix-mode . eglot-ensure)))

;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;; flycheck
(use-package flycheck :straight t
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

(use-package yasnippet :straight t
  :init
  (use-package yasnippet-snippets :straight t))

;; Languages
(use-package yaml-mode :straight t
  :config
  (setq yas-indent-line 'fixed)
  :mode ("\\.sls\\'" . yaml-mode))

(use-package highlight-indent-guides :straight t
  :config
  (setq highlight-indent-guides-method 'character)
  :hook (yaml-mode . highlight-indent-guides-mode))

(use-package jsonnet-mode :straight t)

(use-package python
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
  (setq-local compile-command (concat "python " buffer-file-name))
  :bind (:map python-mode-map
              ("C-c C-c" . recompile)))

;;SUDO-EDIT
(use-package sudo-edit :straight t)

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

(use-package ledger-mode :straight t)

(use-package chatgpt-shell :straight t
  :config
   :custom
   ((chatgpt-shell-openai-key
     (lambda ()
       (auth-source-pass-get 'secret "dev/openai-key")))))

(use-package dall-e-shell :straight t)

(use-package copy-as-format :straight t)

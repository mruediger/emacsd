(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-editor)
(require 'init-global-keybindings)
(require 'init-email)

;;
;; Setup Package Management
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
(require 'use-package)

;;
;; Setup UI
;;
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; cleanup user interface - remove unneded fluff
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(menu-bar-mode -1)     ; Disable menu bar
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-time-mode t)

(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "FontAwesome"))

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 120)

(setq frame-title-format "emacs: %b")

;;split windows proportionally
(setq window-combination-resize 't)

;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)

(use-package which-key :straight t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

(use-package solarized-theme :straight t
  :init
  (setq solarized-scale-org-headlines nil
        solarized-use-variable-pitch nil
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)

  (load-theme 'solarized-light t)

  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

(use-package moody :straight t
  :init
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

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
  (use-package ob-http :straight t)
  (use-package ob-graphql :straight t)
  (use-package ox-hugo :straight t)

  (add-to-list 'org-modules 'org-tempo t)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell      . t)
			         (emacs-lisp . t)
			         (python     . t)
                                 (R          . t)
                                 (gnuplot    . t)
                                 (http       . t)
                                 (graphql    . t)
                                 (js         . t)))

  (defun org-babel-execute:json (body params)
    (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))

  (setq org-confirm-babel-evaluate nil)
  ;; Pretty code blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (setq org-duration-format (quote h:mm))

  (setq org-lowest-priority ?E)
  (setq org-default-priority ?E)

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
  (defun org-open-today () (interactive) (find-file (concat "~/org/today/" (format-time-string "%Y-%m-%d") ".org")))
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

(use-package forge :straight t
  :after magit)

;;
;; Development
;;
(use-package eglot
  :config
  (defun my-eglot-organize-imports () (interactive)
	 (eglot-code-actions nil nil "source.organizeImports" t))

  (add-to-list 'eglot-server-programs '(terraform-mode . ("terraform-ls" "serve")))
  (add-hook 'eglot-managed-mode-hook (lambda ()
                                       (add-hook 'before-save-hook #'my-eglot-organize-imports nil t)
                                       (add-hook 'before-save-hook #'eglot-format-buffer nil t)))
  :hook
  (nix-mode . eglot-ensure)
  (terraform-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure))

;;
;; Nix
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

;; flycheck
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

;;SUDO-EDIT
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

(use-package chatgpt-shell :straight t
  :config
  :custom
  ((chatgpt-shell-openai-key
    (lambda ()
      (auth-source-pass-get 'secret "dev/openai-key")))))

(use-package dall-e-shell :straight t)

(use-package copy-as-format :straight t)

(use-package use-package-chords
  :straight t
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

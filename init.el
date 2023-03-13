;;
;; PACKAGE MANAGEMENT
;;
(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t
	  use-package-always-defer t)

(require 'use-package)

(use-package auto-package-update
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;;
;; LOOK AND FEEL
;;

;; User Interface
(setq inhibit-startup-screen t
      initial-scratch-message nil)

;; cleanup user interface - remove unneded fluff
(scroll-bar-mode -1)   ; Disable visible scrollbar
(tool-bar-mode -1)     ; Disable toolbar
(tooltip-mode -1)      ; Disable tooltips
(menu-bar-mode -1)     ; Disable menu bar
(blink-cursor-mode -1) ;

(setq frame-title-format "emacs: %b")

;;split windows proportionally
(setq window-combination-resize 't)

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 130)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(use-package solarized-theme
  :init
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil)
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; nice mode-line
(use-package moody
  :init
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function)
  :config
  (setq x-underline-at-descent-line t))


;; Behaviour
;; don't create tilde files (~)
(setq make-backup-files nil)

;;always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; INPUT SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; Show keybindings with which-key
(use-package which-key
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.5))

;; MOUSE
(setq mouse-autoselect-window nil)
(setq mouse-yank-at-point t)
(setq make-pointer-invisible nil)

;; KEYBINDINGS
(keymap-global-set "C-z" 'undo)
(keymap-global-set "C-x C-z" 'nil)
(keymap-global-set "<insert>" 'nil)
(keymap-global-set "C-b" nil)
(keymap-global-set "C-x C-b" nil)
(keymap-global-set "C-x b" nil)
(keymap-global-set "C-b C-p" 'switch-to-prev-buffer)
(keymap-global-set "C-b b" 'switch-to-buffer)
(keymap-global-set "C-b C-b" 'ibuffer)
(keymap-global-set "C-b C-k" 'kill-buffer)
(keymap-global-set "C-b C-s" (lambda() (interactive) (switch-to-buffer "*scratch*")))
(keymap-global-set "C-s" 'isearch-forward)
(keymap-global-set "C-M-s" 'isearch-backward)
(keymap-global-set "C-S-s" 'isearch-backward)
(keymap-global-set "C-r" 'query-replace)
(keymap-global-set "C-o" 'other-window)
(keymap-global-set "C-x o" 'nil)
(keymap-global-set "S-DEL" 'kill-whole-line)
(keymap-global-set "M-1" 'delete-other-windows)
(keymap-global-set "M-2" 'split-window-below)
(keymap-global-set "M-3" 'split-window-right)
(keymap-global-set "M-0" 'delete-window)
(keymap-set ctl-x-map "v" 'nil)

;;
;; Editing Config
;;
(setq make-backup-files nil)
(setq-default tab-width 4)
(line-number-mode)
(column-number-mode)
(display-time-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;
;; Server Mode
;;

;; enable emacs server so that emacsclient calls are routed here
(use-package edit-server
  :if window-system
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;;
;; Org Mode
;;
(use-package org
  :init
  (defun org-agenda-show-agenda-and-todo () (interactive) (org-agenda nil "c"))
  (defun mr/org-open-inbox () (interactive)
	(find-file "~/org/inbox.org"))
  :config
  ;; add <s support
  (add-to-list 'org-modules 'org-tempo t)
  (org-babel-do-load-languages 'org-babel-load-languages
			                   '((shell      . t)
				                 (emacs-lisp . t)
				                 (python     . t)
                                 (R          . t)
                                 (gnuplot    . t)))
  (setq org-confirm-babel-evaluate nil)
  (setq org-duration-format (quote h:mm))
  ;; Pretty code blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)
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
  :hook (org-mode . visual-line-mode)
  :bind
  (("C-x o a" . org-agenda-show-agenda-and-todo)
   ("C-x o t" . org-todo-list)
   ("C-x o c" . org-capture)
   ("C-x o i" . mr/org-open-inbox)))

;; direnv
(use-package direnv
  :init
  (direnv-mode))

;;
;; Development
;;
(use-package magit
  :bind
  (("C-x g s" . magit-status)
   ("C-x g l b" . magit-log-buffer-file)
   ("C-x g l c" . magit-log-current)
   ("C-x g l a" . magit-log-all)
   ("C-x g p" . magit-push-current-to-pushremote)))

;; Stuff
(add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

;; flycheck
(use-package flycheck
  :hook (emacs-lisp . flycheck-mode))

;;LSP
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; Terraform (https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls)
  (setq lsp-disabled-clients '(tfls)
        lsp-terraform-ls-enable-show-reference t
        lsp-semantic-tokens-enable t
        lsp-semantic-tokens-honor-refresh-requests t
        lsp-enable-links t)
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t)
  :hook (lsp-mode . lsp-ui-mode))

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

(use-package yasnippet
  :hook ((lsp-mode . yas-minor-mode)))

;; Languages
(use-package yaml-mode
  :mode ("\\.sls\\'" . yaml-mode))

(use-package terraform-mode
  :hook
  ((terraform-mode . lsp)
   (terraform-mode . terraform-format-on-save-mode)))

(use-package nix-mode
  :config
  (defun nix-update () (interactive)
         (let ((default-directory "/sudo::"))
           (compile "nixos-rebuild switch --flake '/home/bag/src/nixos/nixos-config#'")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update))
  :hook (nix-mode . lsp-deferred))

(use-package gotest)

(use-package go-mode
  :init
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  :config (setq go-test-verbose t)
  :bind (:map go-mode-map ("C-c C-c" . go-test-current-project))
  :hook (go-mode . lsp-deferred))

(use-package jsonnet-mode)

(use-package python
  :init
  (setq-default indent-tabs-mode nil)
  :config
  (setq python-indent-offset 4)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" default))
 '(package-selected-packages
   '(auto-package-update ledger-mode sudo-edit jsonnet-mode go-mode gotest nix-mode terraform-mode yaml-mode corfu lsp-terraform lsp-markdown lsp-origami lsp-ui edit-server which-key direnv origami moody solarized-theme flycheck magit lsp-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

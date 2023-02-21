;; -*- lexical-binding: t; -*-

;;
;; PACKAGE MANAGEMENT
;;

(require 'use-package)

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
(setq window-combination-resize 't)

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 130)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

(use-package solarized-theme
  :config
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
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))
    

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

(use-package origami
  :bind (("C-x o o" . origami-open-node)
	 ("C-x o O" . origami-open-all-nodes)
	 ("C-x o c" . origami-close-node)
	 ("C-x o C" . origami-close-node-recursively)))

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

;;
;; Development
;;
(use-package magit
  :bind*
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
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-which-key-integration t)
  (setq read-process-output-max (* 1024 1024))
  :hook
  ((go-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode)

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

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.
  ;; This is recommended since Dabbrev can be used globally (M-/).
  ;; See also `corfu-excluded-modes'.
  :init
  (global-corfu-mode))


;; Languages
(use-package yaml-mode
  :mode ("\\.sls\\'" . yaml-mode))

(use-package terraform-mode
  :mode "\\.hcl\\'"
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package nix-mode
  :config
  (defun nix-update () (interactive) (let ((default-directory "/sudo::")) (compile "nixos-rebuild switch")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update)))

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'before-save-hook 'lsp-organize-imports)
  (setq go-test-verbose t)
  :bind (:map go-mode-map
			  ("C-c C-c" . go-test-current-project)))

(use-package jsonnet-mode)

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

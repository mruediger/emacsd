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
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable menu bar

(column-number-mode)
(display-time-mode)

(set-face-attribute 'default nil
                    :font "Iosevka"
                    :height 130)

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

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
(define-key global-map (kbd "C-z") 'undo)
(define-key global-map (kbd "C-x C-z") 'nil)
(define-key global-map (kbd "<insert>") 'nil)
(define-key global-map (kbd "C-b") nil)
(define-key global-map (kbd "C-x C-b") nil)
(define-key global-map (kbd "C-x b") nil)
(define-key global-map (kbd "C-b C-p") 'switch-to-prev-buffer)
(define-key global-map (kbd "C-b b") 'switch-to-buffer)
(define-key global-map (kbd "C-b C-b") 'ibuffer)
(define-key global-map (kbd "C-b C-k") 'kill-buffer)
(define-key global-map (kbd "C-b C-s") (lambda() (interactive) (switch-to-buffer "*scratch*")))
(define-key global-map (kbd "C-s") 'isearch-forward)
(define-key global-map (kbd "C-M-s") 'isearch-backward)
(define-key global-map (kbd "C-S-s") 'isearch-backward)
(define-key global-map (kbd "C-r") 'query-replace)
(define-key global-map (kbd "C-o") 'other-window)
(define-key global-map (kbd "C-x o") 'nil)
(define-key global-map (kbd "S-DEL") 'kill-whole-line)

;;
;; Editing Config
;;
(setq
 tab-width 4
 make-backup-files nil)

(use-package origami
  :bind (("C-x o o" . origami-open-node)
	 ("C-x o O" . origami-open-all-nodes)
	 ("C-x o c" . origami-close-node)
	 ("C-x o C" . origami-close-node-recursively)))

;;
;; Server Mode
;;

;; enable emacs server so that emacsclient calls are routed here
(server-start)

;;
;; Org Mode
;;
(use-package org
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
  :hook (org-mode . visual-line-mode))

;;
;; Development
;;

;; GIT
(setq vc-handled-backends nil)
(use-package magit
  :bind*
  (("C-x g s" . magit-status)
   ("C-x g l b" . magit-log-buffer-file)
   ("C-x g l c" . magit-log-current)
   ("C-x g l a" . magit-log-all)
   ("C-x g p" . magit-push-current-to-pushremote)))

;; Projectile

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

;; Languages
(use-package yaml-mode
  :mode ("\\.sls\\'" . yaml-mode))

(use-package terraform-mode
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
  :bind (:map go-mode-map
			  ("C-c C-c" . go-test-current-project)))


(use-package go-test)


;;SUDO-EDIT
(use-package sudo-edit)

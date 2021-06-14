;;
;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(package-initialize)
(setq
   package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("melpa" . "http://melpa.org/packages/"))
   package-archive-priorities '(("melpa-stable" . 1)))

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(load-file "~/.emacs.d/secrets.el")

;;
;; LOOK AND FEEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq
 inhibit-startup-screen t
 initial-scratch-message nil)

(add-to-list 'default-frame-alist '(font . "Iosevka:10"))

(setq-default indent-tabs-mode nil)

(use-package darcula-theme :defer t)
(use-package soft-morning-theme :defer t)
(use-package darktooth-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package base16-theme :defer t)
(use-package gruvbox-theme :defer t)
(use-package solarized-theme
  :defer t
  :init
  (setq solarized-scale-org-headlines nil)
  (setq solarized-use-variable-pitch nil))

(load-theme 'solarized-light 't)



;;(load-theme 'base16-eighties 't)
;;(custom-theme-set-faces
;; 'base16-eighties
;; '(org-document-title ((t (:foreground "#999999" :weight bold :height 1.0)))))

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system (scroll-bar-mode 0))

(setq line-number-mode t)
(setq column-number-mode t)

(display-time)

(setq frame-title-format "emacs: %b")

(setq window-combination-resize 't)

;;
;; INPUT SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

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
(define-key global-map (kbd "S-DEL") 'kill-whole-line)

;;
;; BASIC SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(setq
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 make-backup-files nil
 exec-path (append exec-path '("~/bin")))

(setq-default tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(electric-indent-mode 1)
(savehist-mode 1)

;;
;; SETUP EXTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;;ORG
(use-package org
  :config
  (use-package org-bullets)
  (use-package org-gcal
    :config
    (require 'secrets)
    (setq org-gcal-file-alist '(("ruediger@blueboot.org" .  "~/org/gcal-blueboot.org"))))
  (use-package org-jira
    :config
    (setq org-jira-working-dir "~/allianz/jira")
    (setq org-jira-verbosity 'debug)
    (setq jiralib-url "https://jira.gda.allianz/"))

  (use-package ox-gfm)

  (use-package org-trello
    :config
    (setq org-trello-files '("~/src/justwatch/org/trello.org")))

  ;; (add-to-list 'org-structure-template-alist '("sh" "#+BEGIN_SRC sh\n?\n#+END_SRC"))
  (add-to-list 'org-modules 'org-tempo t)
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((shell      . t)
								 (emacs-lisp . t)
								 (python     . t)
                                 (R          . t)
                                 (gnuplot    . t)))
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))
  (defun org-agenda-show-agenda-and-todo () (interactive) (org-agenda nil "c"))
  :init
  (setq org-src-fontify-natively t   ;; Pretty code blocks
        org-src-tab-acts-natively t
		org-confirm-babel-evaluate nil
        org-return-follows-link t)
  (setq org-adapt-indentation nil ;;do not align with the headline
        org-startup-indented t
        org-indent-mode-turns-on-hiding-stars t
        org-indent-mode-turns-off-org-adapt-indentation t) ;;use indent-mode by default
  (setq org-directory "~/org"
        org-refile-use-outline-path 'file)
  (setq org-agenda-files '("~/org" "~/src/justwatch/org")
        org-agenda-window-setup (quote current-window))
  (setq org-capture-templates
        '(("t" "task" entry (file "inbox.org") "* TODO %?\n")
          ("n" "note" entry (file "inbox.org") "* TODO %?\n %a")))
  (setq org-duration-format (quote h:mm)) ;;format of org-time-report
  (setq org-export-with-toc nil)
  (setq org-agenda-custom-commands
        '(("c" "Agenda and TODO" ((agenda "" ((org-agenda-span 9)
                                              (org-agenda-start-day "-2d")))
                                  (alltodo "" ((org-agenda-todo-ignore-deadlines (quote all))
                                               (org-agenda-todo-ignore-scheduled (quote all))))))))
  :bind*
  (("C-x o a" . org-agenda-show-agenda-and-todo)
   ("C-x o t" . org-todo-list)
   ("C-x o c" . org-capture)))

;;MAGIT
(setq vc-handled-backends nil)
(use-package magit
  :bind*
  (("C-x g s" . magit-status)
   ("C-x g l b" . magit-log-buffer-file)
   ("C-x g l c" . magit-log-current)
   ("C-x g l a" . magit-log-all)
   ("C-x g p" . magit-push-current-to-pushremote)))

;;LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (sh-mode . lsp-deferred)
         (yaml-mode . lsp-deferred)
         (nix-mode . lsp-deferred)
         (terraform-mode . lsp-deferred))
  :config
  (setq tab-always-indent 'complete)
  (setq lsp-headerline-breadcrumb-enable nil)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/run/current-system/sw/bin/terraform-ls" "serve"))
                    :major-modes '(terraform-mode)
                    :server-id 'terraform-ls))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  :commands (lsp lsp-deferred))

;;RUST
(use-package rust-mode
  :config
  (use-package toml-mode)
  (use-package cargo
    :hook ((rust-mode . cargo-minor-mode)))

  (setq rust-format-on-save t)
  (setq lsp-auto-guess-root t)
  (defun cargo-test () (interactive) (compile "cargo test -- --nocapture"))
  (defun cargo-run () (interactive) (compile "cargo run"))
  :bind (:map rust-mode-map
              ("<f6>" . cargo-test)
              ("<f7>" . cargo-run)))

;;GOLANG
(use-package go-mode
  :config
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'lsp-format-buffer)
  (add-hook 'before-save-hook 'lsp-organize-imports)
  (defun go-test () (interactive) (compile "go test -v"))
  (defun go-run () (interactive) (compile (concat "go run " (buffer-file-name))))
  :bind (:map go-mode-map
              ("<f6>" . go-test)
              ("C-c C-c" . go-test)
              ("<f7>" . go-run)))

;;PYTHON
(use-package python
  :config
  (defun python-test () (interactive) (compile (concat "python " (buffer-file-name))))
  (defun python-run () (interactive) (compile (concat "python -m unittest " (buffer-file-name))))
  :bind (:map python-mode-map
	      ("<f6>" . python-test)
	      ("<f7>" . python-run)))

;;LaTeX
(setq latex-run-command "latex")

;;Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook #'visual-line-mode)
  (add-hook 'markdown-mode-hook #'flyspell-mode))

;; YAML
(use-package yaml-mode
  :mode ("\\.sls\\'" . yaml-mode))

;; R-Mode
(use-package ess)

;;RAINBOW MODE (colorize strings that represent colors)
(use-package rainbow-mode)

;;SUDO-EDIT
(use-package sudo-edit)

;;Terraform
(use-package terraform-mode
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package dockerfile-mode)

;;Ledger
(use-package ledger-mode)

(use-package csv-mode)

;;NixOS
(use-package nix-mode
  :config
  (defun nix-update () (interactive) (let ((default-directory "/sudo::")) (compile "nixos-rebuild switch")))
  :bind (:map nix-mode-map ("C-c C-c" . nix-update)))

;; Scala
(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (use-package sbt-mode)
  (defun scala-run () (interactive) (compile "sbt --no-colors run"))
  :bind (:map scala-mode-map ("C-c C-c" . scala-run)))


;; JSON
(use-package json-mode)

;; XKCD
(use-package xkcd
  :config
  (defun xkcd-emacs () (interactive) (xkcd-get 378)))

(use-package vagrant-tramp)

;;
;; HELPER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun mr/reload-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun mr/indent-buffer ()
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))
(put 'dired-find-alternate-file 'disabled nil)

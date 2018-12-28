;;
;; PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(package-initialize)
(setq
   package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                      ("org" . "http://orgmode.org/elpa/")
                      ("melpa" . "http://melpa.org/packages/")
                      ("melpa-stable" . "http://stable.melpa.org/packages/"))
   package-archive-priorities '(("melpa-stable" . 1)))

(unless (package-installed-p 'use-package)
        (package-refresh-contents)
        (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)
(setq use-package-always-ensure t)

;;
;; LOOK AND FEEL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(setq
 inhibit-startup-screen t
 initial-scratch-message nil)

(let ((font-code "SpaceMono-10"))
  (add-to-list 'default-frame-alist (cons 'font font-code))
  (set-frame-font font-code))

(use-package darcula-theme :defer t)
(use-package soft-morning-theme :defer t)
(use-package darktooth-theme :defer t)
(use-package zenburn-theme :defer t)
(use-package solarized-theme :defer t)

(load-theme 'solarized-light 't)

(use-package powerline)
(powerline-default-theme)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when window-system (scroll-bar-mode 0))

(setq line-number-mode t)
(setq column-number-mode t)

(display-time)

(setq frame-title-format "emacs: %b")

;;
;; INPUT SETTINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;; MOUSE
(when window-system
  (setq mouse-autoselect-window t))

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

(setq tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(electric-indent-mode 1)
(savehist-mode 1)

;;
;; SETUP INTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;


;;
;; SETUP EXTERNAL PACKAGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;FLYCHECK
(use-package flycheck
  :init (global-flycheck-mode))

;;COMPANY
(use-package company
  :config
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common))

;;SCALA
(use-package scala-mode
  :defer t
  :pin melpa-stable
  :init
  ;; disable ugly implicit underlining
  (setq ensime-sem-high-faces
        '((implicitConversion nil)
          (implicitParams nil))))

(use-package ensime
  :defer t
  :pin melpa-stable)

(use-package sbt-mode
  :defer t
  :pin melpa-stable)

;;IDO
;;(use-package flx-ido
;;  :init
;;  (setq
;;   ido-enable-flex-matching t
;;   ido-everywhere t
;;   ido-default-buffer-method 'selected-window)
;;  :config
;;  (ido-mode t)
;;  (ido-everywhere t)
;;  (flx-ido-mode t))


;;MWIM
(use-package mwim
  :defer t
  :init
  :bind (("C-a" . mwim-beginning-of-code-or-line)
	 ("C-e" . mwim-end-of-code-or-line)))


;;MAGIT
(setq vc-handled-backends nil)
(use-package magit
  :init
  :bind* (("C-x g s" . magit-status)
	  ("C-x g l b" . magit-log-buffer-file)
	  ("C-x g l c" . magit-log-current)
	  ("C-x g l a" . magit-log-all)
	  ("C-x g p" . magit-push-current-to-pushremote)))

;;PYTHON
(use-package python
  :config
  (defun python-test () (interactive) (compile (concat "python " (buffer-file-name))))
  (defun python-run () (interactive) (compile (concat "python -m unittest " (buffer-file-name))))
  :bind (:map python-mode-map
	      ("<f6>" . python-test)
	      ("<f7>" . python-run)))

;;GOLANG
(use-package go-mode
  :init
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (defun go-test () (interactive) (compile "go test"))
  (defun go-run () (interactive) (compile (concat "go run " (buffer-file-name))))
  :bind (:map go-mode-map
	      ("<f6>" . go-test)
	      ("<f7>" . go-run)))

;;RUST
(use-package rust-mode
  :init
  :config
  (use-package company-racer)
  (use-package flycheck-rust)
  (use-package racer
	:config
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
	(setq racer-rust-src-path (concat (getenv "HOME")
		  "/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")))

  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (defun cargo-test () (interactive) (compile "cargo test -- --nocapture"))
  (defun cargo-run () (interactive) (compile "cargo run"))
  :bind (:map rust-mode-map
              ("<f6>" . cargo-test)
              ("<f7>" . cargo-run)))

;;LaTeX
(setq latex-run-command "pdflatex")

;;LUA
(use-package lua-mode)

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

;;Ledger
(use-package ledger-mode)

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

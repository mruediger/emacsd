(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-ui)
(require 'init-editor)
(require 'init-global-keybindings)
(require 'init-email)

(require 'module-go)
(require 'module-magit)
(require 'module-nix)
(require 'module-terraform)

;;
;; PACKAGE MANAGEMENT
;;
(setq use-package-always-defer t)
(require 'use-package)


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
                                             (gnuplot    . t)
                                             (js         . t)))
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

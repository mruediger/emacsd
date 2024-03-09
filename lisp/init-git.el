(use-package magit :straight t
  :bind
  (("C-x g"     . nil)
   ("C-x g s"   . magit-status)
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

(use-package diff-hl
  :straight t
  :hook (find-file . diff-hl-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :hook (vc-dir-mode . diff-hl-dir-mode)
  :hook (diff-hl-mode . diff-hl-flydiff-mode)
  :hook (magit-pre-refresh . diff-hl-magit-pre-refresh)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :custom
  (diff-hl-draw-borders nil))

(provide 'init-git)

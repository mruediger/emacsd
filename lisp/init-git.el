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

(use-package git-link
  :straight t)

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

(use-package nerd-icons-dired

  :hook
  (dired-mode . nerd-icons-dired-mode))

;;(use-package dired-git-log
;;
;;  :hook
;;  (dired-after-readin . dired-git-info-auto-enable))

;;(use-package dired-git-info
;;
;;  :hook
;;  (dired-after-readin . dired-git-info-auto-enable))

;;(use-package dirvish
;;
;;  :custom
;;  (dirvish-attributes '(subtree-state nerd-icons file-size vc-state git-msg))
;;  :config
;;  (dirvish-override-dired-mode 1))

(provide 'init-files)

(use-package dired
  :config
  (defun mr/k-apply-gitops (file-list)
    (interactive (list (dired-get-marked-files)) dired-mode)
    (dired-do-shell-command "kubectl --context gke_rennsport-observability_europe-west1_monitoring-gke apply -f " nil file-list))
  (setq dired-dwim-target t)
  :bind
  (:map dired-mode-map
        ("<left>" . dired-up-directory)
        ("<right>" . dired-find-file)))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'module-dired)

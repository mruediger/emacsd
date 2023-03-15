(keymap-global-set "C-x g"     'nil)
(keymap-global-set "C-x g s"   'magit-status)
(keymap-global-set "C-x g p"   'magit-push-current-to-pushremote)
(keymap-global-set "C-x g l b" 'magit-log-buffer-file)
(keymap-global-set "C-x g l c" 'magit-log-current)
(keymap-global-set "C-x g l a" 'magit-log-all)

(provide 'module-magit)

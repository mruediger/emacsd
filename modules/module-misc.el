(defun terminal-here ()
  (interactive)
  (start-process "foot" nil "foot"))

(keymap-global-set "C-x t" 'terminal-here)

(provide 'module-misc)

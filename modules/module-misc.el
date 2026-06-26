(defun terminal-here ()
  (interactive)
  (start-process "foot" nil "swaymsg" "exec" (concat "foot -D " default-directory)))

(keymap-global-set "C-x t" 'terminal-here)

(provide 'module-misc)

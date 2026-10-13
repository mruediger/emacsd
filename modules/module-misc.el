;;; module-misc.el -*- lexical-binding: t; -*-

(defun terminal-here ()
  (interactive)
  (start-process "foot" nil "swaymsg" "exec" (concat "foot -D " (shell-quote-argument (expand-file-name default-directory)))))

(keymap-global-set "C-x t" 'terminal-here)

(provide 'module-misc)

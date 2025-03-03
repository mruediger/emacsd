(setq make-backup-files nil)

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;;always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Newline at end of file
(setq require-final-newline t)

;;save minibuffer history periodically and when exiting Emacs
(savehist-mode 1)

(provide 'init-editor)

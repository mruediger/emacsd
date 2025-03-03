(setq user-full-name "Mathias Rüdiger")
(setq user-mail-address "ruediger@blueboot.org")

(setq send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-verb t)

(add-hook 'mail-mode-hook 'visual-line-mode)
(add-hook 'mail-mode-hook 'flyspell-mode)

(provide 'init-email)

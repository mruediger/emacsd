(setq
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-stream-type 'starttls
 smtpmail-smtp-service 587
 mail-user-agent 'message-user-agent
 user-mail-address "Sam.Halliday@gmail.com"
 send-mail-function 'smtpmail-send-it
 message-auto-save-directory (concat user-emacs-directory "drafts")
 message-kill-buffer-on-exit t
 message-signature "Best regards,\nSam\n"
 notmuch-search-line-faces '(("unread" :weight bold)
                             ("flagged" :inherit 'font-lock-string-face))
 notmuch-fcc-dirs nil
 notmuch-search-oldest-first nil
 notmuch-address-command "notmuch-addrlookup"
 notmuch-saved-searches '((:name "inbox" :key "i" :query "tag:inbox")
                          (:name "unread" :key "u" :query "tag:unread")
                          (:name "flagged" :key "f" :query "tag:flagged")
                          (:name "drafts" :key "d" :query "tag:draft")
                          (:name "all" :key "a" :query "*")))
(use-package notmuch
  :commands notmuch
  :config
  (add-hook 'message-setup-hook #'company-mode)
  ;; BUG https://debbugs.gnu.org/cgi/bugreport.cgi?bug=23747
  (add-hook 'message-setup-hook #'mml-secure-sign-pgpmime)
  )

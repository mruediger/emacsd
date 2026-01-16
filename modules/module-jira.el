(use-package org-jira :straight t
 :defer t
 :config
 (setq org-jira-working-dir "/home/bag/src/rennsport/jira")
 (setq jiralib-url (auth-source-pass-get "url" "rennsport/jira-api-token"))
 (setq jiralib-user (auth-source-pass-get 'user "rennsport/jira-api-token"))
 (setq jiralib-token
       (cons "Authorization"
             (concat "Basic "
                     (base64-encode-string
                      (format "%s:%s"
                              (auth-source-pass-get 'user "rennsport/jira-api-token")
                              (auth-source-pass-get 'secret "rennsport/jira-api-token"))
                      t))))
 (setq org-jira-default-jql "(assignee = currentUser() or reporter = currentUser()) and resolution = unresolved ORDER BY
  priority DESC, created ASC")
 (setq org-jira-custom-jqls
       '((:jql "project=RS AND labels=Infra"
               :limit 50
               :filename "infra")
         (:jql "project=RS AND (assignee IS EMPTY OR assignee = currentUser())"
               :limit 50
               :filename "mine-or-unassigned")))
 :bind ("C-x j r" . org-jira-get-issues))

;;(use-package jira
;;  :straight (:host github :repo "unmonoqueteclea/jira.el")
;;  :defer t
;;  :config
;;  (setq jira-base-url (auth-source-pass-get "url" "rennsport/jira-api-token"))
;;  (setq jira-username (auth-source-pass-get 'user "rennsport/jira-api-token"))
;;  (setq jira-token (auth-source-pass-get 'secret "rennsport/jira-api-token"))
;;  (setq jira-token-is-personal-access-token nil)
;;  (setq jira-users-max-results 2000)
;;  (setq jira-debug t)
;;  (setq jira-api-version 3))

(provide 'module-jira)

(use-package org :straight t
  :config
  (use-package org-trello :straight (:build (:not compile)))
  (use-package ob-http :straight t)
  (use-package ob-graphql :straight t)
  (use-package ox-hugo :straight t)

  (add-to-list 'org-modules 'org-tempo t)
  :mode ("\\.t?org\\'" . org-mode)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell      . t)
			         (emacs-lisp . t)
			         (python     . t)
                                 (R          . t)
                                 (gnuplot    . t)
                                 (http       . t)
                                 (graphql    . t)
                                 (js         . t)))

  (defun org-babel-execute:json (body params)
    (let ((jq (cdr (assoc :jq params)))
        (node (cdr (assoc :node params))))
    (cond
     (jq
      (with-temp-buffer
        ;; Insert the JSON into the temp buffer
        (insert body)
        ;; Run jq command on the whole buffer, and replace the buffer
        ;; contents with the result returned from jq
        (shell-command-on-region (point-min) (point-max) (format "jq -r \"%s\"" jq) nil 't)
        ;; Return the contents of the temp buffer as the result
        (buffer-string)))
     (node
      (with-temp-buffer
        (insert (format "const it = %s;" body))
        (insert node)
        (shell-command-on-region (point-min) (point-max) "node -p" nil 't)
        (buffer-string))))))

  (setq org-confirm-babel-evaluate nil)
  ;; Pretty code blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (setq org-duration-format (quote h:mm))

  (setq org-lowest-priority ?E)
  (setq org-default-priority ?E)

  (setq org-directory "~/org"
        org-refile-use-outline-path 'file)
  (setq org-agenda-files '("~/org")
        org-agenda-window-setup (quote current-window))
  (setq org-capture-templates
        '(("t" "task" entry (file "inbox.org") "* TODO %?\n")
          ("n" "note" entry (file "inbox.org") "* TODO %?\n %a")))
  (setq org-export-with-toc nil)
  (setq org-agenda-custom-commands
        '(("c" "Agenda and TODO" ((agenda "" ((org-agenda-span 9)
                                              (org-agenda-start-day "-2d")))
                                  (alltodo "" ((org-agenda-todo-ignore-deadlines (quote all))
                                               (org-agenda-todo-ignore-scheduled (quote all))))))))

  (defun org-agenda-show-agenda-and-todo () (interactive) (org-agenda nil "c"))
  (defun org-open-inbox () (interactive) (find-file "~/org/inbox.org"))
  (defun org-open-today () (interactive) (find-file (concat "~/org/today/" (format-time-string "%Y-%m-%d") ".torg")))
  :bind
  ("C-x o a" . org-agenda-show-agenda-and-todo)
  ("C-x o t" . org-todo-list)
  ("C-x o c" . org-capture)
  ("C-x o i" . org-open-inbox)
  ("C-x o n" . org-open-today)
  :hook
  (org-mode . visual-line-mode))

(provide 'init-org)

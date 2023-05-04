(require 'org)

;; add <s shortcut
(add-to-list 'org-modules 'org-tempo t)
(org-babel-do-load-languages 'org-babel-load-languages
			     '((shell      . t)
			       (emacs-lisp . t)
			       (python     . t)
                               (R          . t)
                               (gnuplot    . t)
                               (js         . t)))
(setq org-confirm-babel-evaluate nil)
;; Pretty code blocks
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t)


(setq org-duration-format (quote h:mm))

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

(add-hook 'org-mode-hook 'visual-line-mode)

(defun org-agenda-show-agenda-and-todo () (interactive) (org-agenda nil "c"))
(defun org-open-inbox () (interactive) (find-file "~/org/inbox.org"))
(defun org-open-today () (interactive) (find-file (concat "~/org/today-" (format-time-string "%Y-%m-%d") ".org")))

(keymap-global-set "C-x o a" 'org-agenda-show-agenda-and-todo)
(keymap-global-set "C-x o t" 'org-todo-list)
(keymap-global-set "C-x o c" 'org-capture)
(keymap-global-set "C-x o i" 'org-open-inbox)
(keymap-global-set "C-x o n" 'org-open-today)

(provide 'module-org)

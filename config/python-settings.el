(require 'python)

(defun python-test ()
  (interactive)
  (compile "nosetests -s"))

(defun python-run ()
  (interactive)
  (compile (concat "python " buffer-file-name)))

(add-hook 'python-mode-hook
          (lambda() (local-set-key (kbd "<f6>") 'python-test)))
(add-hook 'python-mode-hook
          (lambda() (local-set-key (kbd "<f7>") 'python-run)))

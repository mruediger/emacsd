(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(defun scalatest()
  (interactive)
  (compilation-start "sbt test"))

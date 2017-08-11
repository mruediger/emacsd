(use-package scala-mode
  :defer t
  :ensure t
  :pin melpa)
  
(use-package ensime
  :defer t  
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :defer t
  :ensure t
  :pin melpa
  :init
  (setq sbt:program-name "sbt -mem 2048"))

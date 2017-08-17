(use-package expand-region :ensure t)
(use-package mark-multiple :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("M->" . mc/mark-next-like-this)
         ("M-<" . mc/mark-previous-like-this)))
  
(use-package popup-imenu
  :ensure t
  :commands popup-imenu
  :bind ("M-i" . popup-imenu))


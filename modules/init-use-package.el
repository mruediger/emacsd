(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("org" . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")))

(use-package use-package
  :config
  (setq use-package-always-ensure t))


(provide 'init-use-package)

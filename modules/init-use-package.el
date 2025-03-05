(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("org" . "https://orgmode.org/elpa/")
	("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(provide 'init-use-package)

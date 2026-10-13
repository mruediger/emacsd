;;; module-dart.el -*- lexical-binding: t; -*-

(use-package dart-mode
  :hook
  (dart-mode . eglot-ensure))

(use-package flutter
  :after dart-mode)

(provide 'module-dart)

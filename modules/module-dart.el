(use-package dart-mode
  :straight t
  :hook
  (dart-mode . eglot-ensure))

(use-package flutter
  :straight t
  :after dart-mode)

(provide 'module-dart)

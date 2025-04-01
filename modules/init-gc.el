;; Garbage Collector Magic Hack
;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with user activity.
;; During normal use a high GC threshold is set.
;; When idling GC is triggered and a low threshold is set.
;; A more detailed explanation of the rationale behind this can be found at:
;; http://akrl.sdf.org/
(use-package gcmh
 
  :hook (after-init . gcmh-mode))

(provide 'init-gc)

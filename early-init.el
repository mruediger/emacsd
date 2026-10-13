;;; early-init.el -*- lexical-binding: t; -*-

;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.

;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq gc-cons-threshold most-positive-fixnum)

;; Packages are provided by nix (home-manager). Keep `package-enable-at-startup'
;; at its default so package.el activates them (loads their autoloads).

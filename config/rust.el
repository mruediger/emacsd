(defconst rust-src-path
  "/home/bag/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")

(use-package racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (setq racer-rust-src-path rust-src-path))

(defun cargo-test ()
  (interactive)
  (compile "cargo test"))

(defun cargo-run ()
  (interactive)
  (compile "cargo run"))

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  :bind (("<f6>" . cargo-test)
         ("<f7>" . cargo-run)))
  

;; todo
;; https://github.com/flycheck/flycheck-rust

;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

(use-package rustic
  :custom
  (rustic-spinner-type "moon")
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-analyzer-command '("rust-analyzer"))
  (setenv "RUST_BACKTRACE" "full")

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

;;; posimacs-rust.el ends here.

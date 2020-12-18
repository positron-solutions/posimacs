;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

(use-package rustic
  :requires (direnv)
  :init
  ; we need RUST_SRC_PATH from direnv before lsp starts
  (advice-add 'rustic-setup-lsp :before #'direnv-update-environment)
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-analyzer-command "rust-analyzer")
  (setenv "RUST_BACKTRACE" "full"))

;;; posimacs-rust.el ends here

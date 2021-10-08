;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

(use-package rustic
  :custom
  (rustic-spinner-type "moon")
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-analyzer-command "rust-analyzer")
  (setenv "RUST_BACKTRACE" "full")

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

;;; posimacs-rust.el ends here

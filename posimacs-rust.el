;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

;; Tree sitter support is installed 😀

(use-package rust-ts-mode
  :ensure nil)              ; vestigal

(use-package rust-mode
  :hook ((rust-mode .  electric-pair-local-mode)
         (rust-mode .  lsp-deferred))
  :init
  (setenv "RUST_BACKTRACE" "full")
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  (defun pmx--rustfmt-use-leptos (orig-func &rest r)
    (if-let* ((leptosfmt (executable-find "leptosfmt")))
        (let ((rust-rustfmt-bin "leptosfmt")
              (rust-rustfmt-switches '("--stdin" "--tab-spaces" "2"  "--rustfmt")))
          (apply orig-func r))
      (apply orig-func r)))
  (advice-add #'rust-format-buffer :around #'pmx--rustfmt-use-leptos))

;;; posimacs-rust.el ends here.

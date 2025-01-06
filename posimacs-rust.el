;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

;; Tree sitter support is installed 😀

(use-package rust-ts-mode
  :ensure nil)              ; vestigal

(use-package rust-mode
  :hook ((rust-mode .  electric-pair-local-mode)
         ;; (rust-mode .  lsp-deferred)
         (rust-mode . eglot-ensure)
         )
  :init
  (setenv "RUST_BACKTRACE" "full")
  (setq rust-mode-treesitter-derive t)
  :config

  (setq eldoc-echo-area-prefer-doc-buffer 'maybe)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq echo-area-display-truncation-message nil)

  ;; (setopt lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (setopt lsp-rust-analyzer-display-chaining-hints t)
  ;; (setopt lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (setopt lsp-rust-analyzer-display-closure-return-type-hints t)n

  (defun pmx--rust-tweaks ()
    (auto-fill-mode -1)
    (electric-pair-local-mode -1))
  (add-hook 'rust-mode-hook #'pmx--rust-tweaks)
  (setq rust-format-on-save t)
  (defun pmx--rustfmt-use-leptos (orig-func &rest r)
    (if-let* ((leptosfmt (executable-find "leptosfmt")))
        (let ((rust-rustfmt-bin "leptosfmt")
              (rust-rustfmt-switches '("--stdin" "--tab-spaces" "2"  "--rustfmt")))
          (apply orig-func r))
      (apply orig-func r)))
  (advice-add #'rust-format-buffer :around #'pmx--rustfmt-use-leptos))

;;; posimacs-rust.el ends here.

;;; posimacs-rust.el --- Rust -*-lexical-binding: t-*-

;;; Commentary:
;; Rust support

;;; Code:

;; Tree sitter support is installed 😀

(use-package rust-ts-mode
  :ensure nil)              ; vestigal

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (add-hook 'rust-mode-hook #'eglot-ensure)

  ;; (setq rust-rustfmt-bin "leptosfmt")
  ;; (setq lsp-rust-rustfmt-path "leptosfmt")

  ;; TODO eldoc settings
  (setq eldoc-echo-area-prefer-doc-buffer 'maybe)
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eglot-code-action-indications '(margin))

  ;; TODO echo area
  ;; (setq echo-area-display-truncation-message nil)

  ;; (setopt lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (setopt lsp-rust-analyzer-display-chaining-hints t)
  ;; (setopt lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (setopt lsp-rust-analyzer-display-closure-return-type-hints t)

  (defun pmx--rust-tweaks ()
    (setenv "RUST_BACKTRACE" "full")
    (auto-fill-mode 1)
    (setq-local fill-column 100)
    (electric-pair-local-mode 1))
  (add-hook 'rust-mode-hook #'pmx--rust-tweaks)

  (with-eval-after-load 'eglot
    (defvar rust-cargo-features ["all"])

    (defun pmx-rust-set-features ()
      (interactive
       (let* ((all (shell-command-to-string "cargo read-manifest | jq  -r '.features | to_entries | .[] | \"\\(.key)  \\(.value | join(\"\"))\"'"))
              (all (concat "all\n" all))
              (features (string-split all "\n" t "[[:space:]]+"))
              (selected (completing-read-multiple "Select features: " features))
              (features (vconcat (mapcar (lambda (s) (car (string-split s " ")))
                                         selected))))
         (prog1 nil (setq rust-cargo-features features))))

      ;; TODO, this indirectly passes new features through the variable
      (let ((settings (if (equal (aref rust-cargo-features 0) "all")
                          `("rust-analyzer"
                            :initializationOptions
                            (:cargo (:allFeatures t)))
                        `("rust-analyzer"
                          :initializationOptions
                          (:cargo (:features ,rust-cargo-features))))))
        (setcar
         (let* ((value (alist-get '(rust-ts-mode rust-mode) eglot-server-programs nil
                                  nil 'equal))
                (entry (cons '(rust-ts-mode rust-mode) value)))
           (member entry eglot-server-programs))
         `((rust-ts-mode rust-mode) . ,settings)))

      (when-let ((server (eglot-current-server)))
        ;; For some reason the running server indicates error when being shut down,
        ;; but it does shut down.
        (ignore-errors (eglot-shutdown server)))
      (sleep-for 1)
      (message "%S" (car eglot-server-programs))
      (eglot-ensure))

    (pmx-rust-set-features)

    ;;  Don't spam yourself
    (setq eglot-send-changes-idle-time 3.0))

  (defun pmx--rustfmt-use-leptos (orig-func &rest r)
    (if-let* ((leptosfmt (executable-find "leptosfmt")))
        (let ((rust-rustfmt-bin "leptosfmt")
              (rust-rustfmt-switches '("--stdin" "--tab-spaces" "2"  "--rustfmt")))
          (apply orig-func r))
      (apply orig-func r)))
  (advice-add #'rust-format-buffer :around #'pmx--rustfmt-use-leptos)
  (setopt rust-format-on-save t))

(use-package eglot-booster
  :after eglot
  :ensure (eglot-booster
           :host github
           :repo "jdtsmith/eglot-booster")
  :config (eglot-booster-mode))

;;; posimacs-rust.el ends here.

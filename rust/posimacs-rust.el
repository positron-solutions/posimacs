;;; posimacs-rust.el --- Rust

;;; Commentary:
;; Rust support

;;; Code:

;; Tree sitter support is installed 😀
;; TODO This lisp is dependent upon Emacs being installed with tree sitter
;; support, which is in ../posimacs.nix
;; It is a dependency, and stating it within the Rust module would require some
;; option support, from the top-level module, and modules add complexity...
;; Possibly we can configure Emacs to load the tree sitter grammars without
;; having them in the Emacs derivation itself.

(setq major-mode-remap-alist
      '((rust-mode . rust-ts-mode)))

(use-package rust-ts-mode
  :hook (rust-ts-mode . eglot-ensure)
  :elpaca nil
  :init
  (setenv "RUST_BACKTRACE" "full"))

;;; posimacs-rust.el ends here.

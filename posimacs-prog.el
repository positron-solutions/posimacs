;;; posimacs-prog --- Prog modes

;;; Commentary:
;;
;; Prog mode is a pseudo mode that includes all programming modes.  These
;; modules add the langauge server integration and then specific language
;; settings to provide more advanced refactoring and type aware IDE behavior
;;

;;; Code:

(use-package nix-mode)

(use-package yasnippet
  :config (yas-global-mode))

(use-package rustic
  :requires (direnv)
  :init
  (advice-add 'rustic-setup-lsp :before #'direnv-update-environment)
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-analyzer-command "rust-analyzer")
  (setenv "RUST_BACKTRACE" "full"))

;; COMPlete ANYthing
(use-package company
  :delight company-mode
  :after (lsp lsp-ui)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-selection-wrap-around t)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-t") 'company-search-toggle-filtering))


;; not compatible with emacs in a tty
(use-package company-box
  :delight (company-box-mode nil company-box)
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :delight flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)) ; Highlight detected errors

(use-package lsp-mode ;; language server protocol
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-prefer-capf t)
  (setq lsp-rust-server 'rust-analyzer) ;; will use rls as backup
  (setq lsp-rust-analyzer-server-command "rust-analyzer"))

;; LSP may send messages that are fairly large
(setq read-process-output-max (* (* 1024 1024) 8)) ;; 8mb

;; excessive UI feedback for light reading between coding
(use-package lsp-ui
  :config
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-imenu-enable nil
   lsp-ui-peek-enable nil
   lsp-ui-sideline-enable nil
   ))

;; TODO this is incomplete for rust
;; debug adapter protocol
(use-package dap-mode
  :config
  ;; (dap-mode 1)
  ;; (dap-ui-mode 1)
  ;; (dap-tooltip-mode 1) ; dap-mode will use minibuffer if not enabled
  ;; (tooltip-mode 1))
  )

;; (use-package dap-lldb)

;; (dap-register-debug-template

;; Editing kubernetes yamls can link directly to API reference
(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

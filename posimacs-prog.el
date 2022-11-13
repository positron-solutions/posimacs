;;; posimacs-prog --- Prog modes

;;; Commentary:
;;
;; Prog mode is a pseudo mode that includes all programming modes.  These
;; modules add the langauge server integration and then specific language
;; settings to provide more advanced refactoring and type aware IDE behavior
;;

;;; Code:

(use-package nix-mode)
(use-package list-environment) ; specificaly for inspecting nix envs

;; LSP may send messages that are fairly large
(setq read-process-output-max (* (* 1024 1024) 32)) ;; 32mb

(use-package lsp-mode
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil) ; only show symbol info, not everything returned
  (lsp-signature-render-documentation nil) ; don't show eldocs in popup
  (lsp-idle-delay 0.5 "Reduce error noise while typing or completing")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-headerline-breadcrumb-enable nil)
  :config
  ;; (setq lsp-use-plists t)  ; breaks rust-analyzer integration right now
  (advice-add 'lsp :before #'direnv-update-environment)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  ; (lsp-ui-peek-enable nil) ; since peek opens on action, not spammy
  (lsp-ui-sideline-enable nil) ; XXX de-spam
  (lsp-ui-doc-include-signature) ; if we did show docs, show signature!
  (lsp-ui-doc-enable nil))

;; COMPlete ANYthing
(use-package company
  :custom
  (company-idle-delay 0.04) ;; how long to wait until popup
  (company-tooltip-minimum 10)
  (company-tooltip-limit 16)

  ;; This binding strategy, together with default behavior, achieves the following points:
  ;; - completions present themselves automatically
  ;; - space and enter both abort completions for the current word
  ;; - tab attempts to complete up to the final value
  :bind ((:map company-active-map
	       ("M-n". company-select-next)
	       ("M-p". company-select-previous)
               ("RET" . nil) ; pass-through newlines even if selections available
               ("<return>" . nil)
               ; ("SPC" . nil)
               ; ("<space>" . nil)
               ("<tab>". company-complete-selection)
	       ("TAB". company-complete-selection)))
  :init
  (setq-default company-minimum-prefix-length 1
                tab-always-indent 'complete
                tab-first-completion 'word-or-paren-or-punct)
  (add-hook 'after-init-hook 'global-company-mode) ; thank me later
  (add-hook 'text-mode-hook (lambda () (setq-local company-minimum-prefix-length 5)))
  :config
  ;; Orderless was really bad at sorting matches for company.  Not using it for
  ;; autocomplete matches until some better priority system is found.
  (defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))
  (advice-add 'company-capf :around #'company-completion-styles))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-prefixes))
  (orderless-component-separator " +-")
  (completion-cycle-threshold 16)
  ;; File completion has been fine with orderless.
  (completion-category-overrides nil))

(use-package yasnippet
  :config
  (setq yas/root-directory (list (expand-file-name "etc/yasnippet/snippets" user-emacs-directory)))
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package flycheck
  :custom (flycheck-display-errors-delay 10 "Reduce error noise while typing")
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(defun pmx-buffer-ert-p (buf act)
  "BUF is an ert results buffer, ignore ACT."
  (eq (buffer-local-value 'major-mode (get-buffer buf)) 'ert-results-mode))

(use-package ert
  :config
  (add-to-list 'display-buffer-alist
               `(pmx-buffer-ert-p         ;predicate
                 (display-buffer-reuse-mode-window) ;functions to try
                 (inhibit-same-window . nil))))

(use-package auto-compile)

(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  ; (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
         :request "launch"
         :name "LLDB::Run"
	 :gdbpath "rust-lldb"
         :target nil
         :cwd nil)))

;;; posimacs-prog.el ends here

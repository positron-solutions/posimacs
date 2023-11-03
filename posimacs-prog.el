;;; posimacs-prog --- Programming mode  -*- lexical-binding: t -*-

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
  :demand t
  :custom
  (company-idle-delay 0.3) ;; how long to wait until popup
  (company-tooltip-minimum 10)
  (company-tooltip-limit 16)
  (company-tooltip-flip-when-above t)
  (company-tooltip-width-grow-only t)
  (company-minimum-prefix-length 3)
  :config

  ;; replace company-capf with merged company and dabbrev :-)
  (setq company-backends
        (mapcar (lambda (b)
                  (if (eq b 'company-capf)
                      '(company-capf :with company-dabbrev-code)
                    b))
                company-backends))

  ;; TODO possibly upstream this
  (defun pmx-company-complete-common-or-finish ()
    "Insert the common part of current candidates or finish."
    (interactive)
    (when (company-manual-begin)
      (let ((tick (buffer-chars-modified-tick)))
        (call-interactively 'company-complete-common)
        (when (eq tick (buffer-chars-modified-tick))
          (let ((result (nth company-selection company-candidates)))
            (company-finish result))))))

  ;; This binding strategy, together with default behavior, achieves the following points:
  ;; - completions present themselves automatically
  ;; - enter aborts completions for the current word
  ;; - tab will either complete the common part or just finish with the first
  ;;   candidate if this further common completion accomplished nothing.
  (define-key company-active-map (kbd "M-n") #'company-select-next)
  (define-key company-active-map (kbd "M-p") #'company-select-previous)
  (define-key company-active-map (kbd "<return>") nil) ; abort & pass through to newline etc
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "<space>") nil) ; abort & pass through
  (define-key company-active-map (kbd "SPC") nil)
  (define-key company-active-map (kbd "<tab>") #'pmx-company-complete-common-or-finish)
  (define-key company-active-map (kbd "TAB") #'pmx-company-complete-common-or-finish)

  (setq-default tab-always-indent t
                tab-first-completion 'word-or-paren-or-punct)

  (add-hook 'prog-mode-hook #'company-mode)

  ;; use company for ielm completions and unhook the oppressive built-in
  ;; completions on tab
  (with-eval-after-load 'ielm
    (add-hook 'ielm-mode-hook #'company-mode)
    (define-key ielm-map (kbd "<tab>") #'company-indent-or-complete-common))

  ;; use less aggressive completion when in text mode
  (add-hook 'text-mode-hook (lambda () (setq-local company-minimum-prefix-length 5))))

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
  (global-flycheck-mode))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))


(provide 'posimacs-prog)
;;; posimacs-prog.el ends here

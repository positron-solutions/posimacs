;;; posimacs-prog --- Programming mode  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Prog mode is a pseudo mode that includes all programming modes.  These
;; modules add the langauge server integration and then specific language
;; settings to provide more advanced refactoring and type aware IDE behavior
;;

;;; Code:

(use-package nixpkgs-fmt)

(use-package list-environment) ; specifically for inspecting nix envs

(use-package elec-pair
  :ensure nil
  :config
  ;; Selected modes opt out
  ;; (electric-pair-local-mode -1)
  (setopt electric-pair-pairs
          '((34 . 34)                   ; "
            (40 . 41)                   ; (
            (91 . 93)                   ; [
            (123 . 125)                 ; {
            (60 . 61))))                ; <

(setenv "LSP_USE_PLISTS" "true")        ; TODO at least one of these is redundant
(use-package lsp-mode
  :defer t
  :init
  ;; LSP may send messages that are fairly large
  (setopt read-process-output-max 1048576) ; max of /proc/sys/fs/pipe-max-size

  :config
  (setopt lsp-use-plists t)             ; see `setenv' above

  ;; what to use when checking on-save. "check" is default, TODO someone
  ;; preferred clippy, but it wasn't me
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")

  (setopt lsp-eldoc-render-all nil) ; only show symbol info, not everything returned
  (setopt lsp-signature-render-documentation nil) ; don't show eldocs in popup
  (setopt lsp-idle-delay 10.0)   ; Reduce error noise while typing or completing
  (setopt lsp-headerline-breadcrumb-enable nil)

  (setopt lsp-rust-analyzer-closure-return-type-hints "always")
  (setopt lsp-rust-analyzer-display-closure-return-type-hints t)

  ;; for local environments from envrc
  (defun pmx--lsp-set-plist (&rest _)
    (setenv "LSP_USE_PLISTS" "true"))
  (advice-add #'lsp-mode :before #'pmx--lsp-set-plist)

  ;; Use LSP completions between triggers and min prefix
  (defun pmx--company-set-prefix-length ()
    (setq-local company-minimum-prefix-length 1))
  (add-hook 'lsp-mode-hook #'pmx--company-set-prefix-length))

;; TODO check some cases where this may be relevant
;; (advice-add #'lsp :before #'envrc--update-env)

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  ;; (lsp-ui-peek-enable nil) ; since peek opens on action, not spammy
  ;; (setopt lsp-ui-sideline-enable nil)
  (setopt lsp-ui-sideline-delay 10.0)
  (setopt lsp-ui-doc-include-signature t) ; if we did show docs, show signature!
  (setopt lsp-ui-doc-enable nil)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; COMPlete ANYthing
(use-package company
  :after orderless
  :custom
  (company-idle-delay 0.3) ;; how long to wait until popup
  (company-tooltip-minimum 8)
  (company-tooltip-limit 16)
  (company-tooltip-flip-when-above nil)
  (company-tooltip-width-grow-only t)
  (company-minimum-prefix-length 3)     ; this is overridden in lsp buffers
  :config
  (setopt company-dabbrev-code-other-buffers t)
  (setopt company-dabbrev-other-buffers t)
  (setopt company-dabbrev-code-everywhere t)
  (setopt company-dabbrev-code-other-buffers t)
  (setopt company-tooltip-maximum-width 80) ; LSP completions are looooong
  (setopt company-selection-wrap-around t)

  (defun just-one-face (fn &rest args)
    (let ((orderless-match-faces [completions-common-part]))
      (apply fn args)))

  ;; (advice-add 'company-capf--candidates :around #'just-one-face)

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
  (add-hook 'text-mode-hook
            (lambda () (setq-local company-minimum-prefix-length 4))))

(use-package lsp-ivy
  :defer t)

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
    :commands (global-flycheck-mode)
    :ensure (flycheck :autoloads t)
    :config
    (setopt flycheck-emacs-lisp-load-path 'inherit)
    (setopt flycheck-display-errors-delay 2.0)
    (setopt flycheck-idle-change-delay 2.0)
    (setopt flycheck-check-syntax-automatically '(save idle-change new-line
                                                  mode-enabled))
    (letrec ((flycheck-init
              (lambda ()
                (global-flycheck-mode)
                (remove-hook 'elpaca-after-init-hook
                             flycheck-init))))
      (add-hook 'elpaca-after-init-hook flycheck-init)))

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; TODO Dape with Rust

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-nix.el
(use-package nix-ts-mode
  :ensure (nix-ts-mode
           :fetcher github
           :repo "remi-gelinas/nix-ts-mode")
  :init (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))
  :hook (nix-ts-mode . eglot-ensure)
  :config

;; Editing kubernetes yamls can link directly to API reference
(use-package k8s-mode
  :elpaca (k8s-mode :autoloads t)
  :hook (k8s-mode . yas-minor-mode))

  (let ((nix-settings
         '((nix-ts-mode) . #'pmx--project-flake-path)))
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs nix-settings)))

  (defun pmx--nixpkgs-fmt-on-save ()
    (add-hook 'before-save-hook #'nixpkgs-fmt-buffer -100 t))
  (add-hook 'nix-ts-mode-hook #'pmx--nixpkgs-fmt-on-save))

(use-package kubernetes
  :defer t
  :ensure (kubernetes :autoloads t))

(use-package yaml-pro
  :elpaca (yaml-pro :autoloads t)
  ;; TODO Set up mode hooks
  )

(provide 'posimacs-prog)
;;; posimacs-prog.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

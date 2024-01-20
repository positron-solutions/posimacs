;;; posimacs-ai.el --- standard speedups & bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Decent setup for secrets and integrating the new hot shits into
;; your daily workflows.

;; Fuller documentation
;; https://www.masteringemacs.org/article/keeping-secrets-in-emacs-gnupg-auth-sources
;;
;; If you want to use a ubikey with GPG to only unpack secrets when
;; the user is present (this only truly works well with agent programs
;; that can perform actions on behalf of a local client without
;; revealing the secret to the local client).
;; https://github.com/drduh/YubiKey-Guide#yubikey
;;
;; Another GPG and Nix guide
;; https://rzetterberg.github.io/yubikey-gpg-nixos.html

;;; Code:

;; Don't use pin-entry within Emacs
(setenv "GPG_AGENT_INFO" nil)

(use-package chatgpt-shell
  :commands (chatgpt-shell)
  :custom
  (setq chatgpt-shell-model-version 6)  ; gpt-3.5 turbo
  (setq chatgpt-shell-system-prompt 2)  ; general
  (setq (chatgpt-shell-openai-key
         (lambda ()
           (auth-source-pick-first-password :host "api.openai.com")))))

(provide 'posimacs-ai)
;;; posimacs-ai.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

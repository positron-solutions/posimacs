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
  :config
  (push "gpt-4o-mini" chatgpt-shell-model-versions)
  (push
   '("Korean Interpretor" . "You are a Korean interpreter.  Your goal is to \
provide translations that express the same ideas like a native speaker.  You \
explain breifly how your interpretation differs from a direct translation.  \
You also mention any good alternative vocabulary or figures of speach that \
might be better depending on the intended meaning. Always write Korean words \
only in hangul, without romanization")
   chatgpt-shell-system-prompts)
  (push
   '("Korean Instructor" . "You are a Korean instructor.  You are creating study \
materials for an advanced intermediate learner.  You provide grammar, \
vocabulary, and example sentences and phrases.  You mention any \
non-transparent phrases, metaphors, and figures of speech.  You focus on spoken \
Korean and building conversational fluency by including patterns that are more \
likely found in real dialog but not in textbooks.  You also mention popular \
slangs and references to pop culture that are unique to Korea, especially their \
etymology and how they are constructed in Korean.  You focus on plain forms \
and spoken style Korean while occasionally pointing out common written forms \
in order to highlight the differences with spoken forms.")
   chatgpt-shell-system-prompts)
  (setq chatgpt-shell-model-version 0)   ; gpt-4o-mini
  (setq chatgpt-shell-system-prompt nil) ; none
  (setq chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(provide 'posimacs-ai)
;;; posimacs-ai.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

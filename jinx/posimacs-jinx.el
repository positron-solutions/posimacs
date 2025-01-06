;;; posimacs-jinx --- Load jinx spellchecking

;;; Commentary:
;;
;; Loads a dynamic module that was installed via home manager module declared in
;; ./flake.nix & ./jinx.nix and included in ../posimacs.nix

;;; Code:

(use-package jinx
  :ensure nil
  :demand t
  :load-path  "~/.emacs.d/vendor/emacs-jinx/"
  :hook (elpaca-after-init-hook . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config
  ;; (setopt jinx-languages "ko en_US-wo_accents en_US-w_accents en_US-variant_1 en_US-variant_0 en_US en_GB-wo_accents en_GB-w_accents en_GB-variant_1 en_GB-variant_0 en_GB-ize-wo_accents en_GB-ize-w_accents en_GB-ize en_GB-ise-wo_accents en_GB-ise-w_accents en_GB-ise en_GB en_CA-wo_accents en_CA-w_accents en_CA-variant_1 en_CA-variant_0 en_CA en_AU-wo_accents en_AU-w_accents en_AU-variant_1 en_AU-variant_0 en_AU en-wo_accents en-w_accents en-variant_2 en-variant_1 en-variant_0 en")
  (setopt jinx-languages "ko en_US en_GB en")
  (setopt jinx-delay 2.0)
  (global-jinx-mode 1))

(provide 'posimacs-jinx)
;;; posimacs-jinx.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

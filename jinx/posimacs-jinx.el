;;; posimacs-jinx --- Load jinx spellchecking -*-lexical-binding: t-*-

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
  (setq jinx-languages "en_US ko")
  (setq jinx-delay 2.0)
  (global-jinx-mode 1))

(provide 'posimacs-jinx)
;;; posimacs-jinx.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

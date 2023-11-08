;;; posimacs-jinx --- Load jinx spellchecking

;;; Commentary:
;;
;; Loads a dynamic module that was installed via home manager module declared in
;; ./flake.nix & ./jinx.nix and included in ../posimacs.nix

;;; Code:

(use-package jinx
  :elpaca nil
  :load-path  "~/.emacs.d/vendor/emacs-jinx/"
  :hook (elpaca-after-init-hook . global-jinx-mode)
  :bind ([remap ispell-word] . jinx-correct)
  :config (global-jinx-mode 1))

(provide 'posimacs-jinx)
;;; posimacs-jinx.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

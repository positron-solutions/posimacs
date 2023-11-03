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
  :bind ([remap ispell-word] . jinx-correct))

(provide 'posimacs-jinx)
;;; posimacs-jinx.el ends here

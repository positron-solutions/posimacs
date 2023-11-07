;;; posimacs-terminal --- Running Normal Things

;;; Commentary:
;;
;; Loads a dynamic module that was installed via home manager module declared in
;; ./flake.nix & ./vterm.nix and included in ../posimacs.nix

;;; Code:

;; TODO path is duplicated with home manager module.
(use-package vterm
  :elpaca nil
  :load-path  "~/.emacs.d/vendor/emacs-vterm"
  :commands (vterm vterm-other-window))

(provide 'posimacs-terminal)
;;; posimacs-terminal.el ends here

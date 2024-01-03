;;; posimacs-terminal --- Running Normal Things

;;; Commentary:
;;
;; Uses Elisp only EAT with it's cool modes I don't understand yet.

;;; Code:

(use-package eat
  :config
  (add-hook 'eat-session-created-hook #'eat-semi-char-mode)
  (keymap-set eat-semi-char-mode-map "M-o" nil)
  (define-obsolete-function-alias 'vterm #'eat "0.1.0"))

(provide 'posimacs-terminal)
;;; posimacs-terminal.el ends here

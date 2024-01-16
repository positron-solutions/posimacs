;;; posimacs-terminal --- Running Normal Things

;;; Commentary:

;; Eat with some shortcuts to allow implicit mode changes.

;;; Code:

(use-package eat
  :after avy
  :config
  (setopt eat-query-before-killing-running-terminal 'auto)
  (add-hook 'eat-session-created-hook #'eat-emacs-mode)
  (keymap-set eat-semi-char-mode-map "M-o" nil)

  (defun pmx--eat-jump-to-line ()
    (interactive)
    (eat-emacs-mode)
    (call-interactively #'avy-goto-word-1))
  (keymap-set eat-semi-char-mode-map "M-j" #'pmx--eat-jump-to-line)

  (defun pmx--eat-return-to-semi-char ()
    (interactive)
    (eat-semi-char-mode)
    (goto-char (point-max)))
  (keymap-set eat-mode-map
              "RET" #'pmx--eat-return-to-semi-char)

  (define-obsolete-function-alias 'vterm #'eat "0.1.0"))

(provide 'posimacs-terminal)
;;; posimacs-terminal.el ends here

;;; posimacs-terminal --- Running Normal Things  -*- lexical-binding: t -*-

;;; Commentary:

;; Eat with some shortcuts to allow implicit mode changes.

;;; Code:

(use-package eat
  :commands (eat eat-project eat-project-other-window)
  :after avy
  :config
  (setopt eat-query-before-killing-running-terminal 'auto)
  (setopt eat-kill-buffer-on-exit t)
  ;; relevant bindings found in posimacs-bindings.el
  (dolist (key '([up] [down] [left] [right]
                 [?\e 111]              ; M-o
                 [?\e 108]              ; M-l
                 [?\e  99]              ; M-s
                 [?\e 122]              ; M-z
                 [?\e 118]              ; M-v
                 ))
    (add-to-list 'eat-semi-char-non-bound-keys key))
  (eat-update-semi-char-mode-map)
  (let ((after-load-alist nil)
        (after-load-functions nil))
    (eat-reload))
  (add-hook 'eat-session-created-hook #'eat-emacs-mode)

  (defun pmx--eat-jump-to-line ()
    (interactive)
    (eat-emacs-mode)
    (call-interactively #'avy-goto-word-1))
  (keymap-set eat-semi-char-mode-map "M-l" #'pmx--eat-jump-to-line)

  (defun pmx--eat-swiper-to-line ()
    (interactive)
    (eat-emacs-mode)
    (call-interactively #'swiper))
  (keymap-set eat-semi-char-mode-map "M-s" #'pmx--eat-swiper-to-line)

  (defun pmx--eat-set-mark-to-line ()
    (interactive)
    (eat-emacs-mode)
    (call-interactively #'set-mark-command))
  (keymap-set eat-semi-char-mode-map "C-SPC" #'pmx--eat-set-mark-to-line)

  (defun pmx--eat-return-to-semi-char ()
    (interactive)
    (eat-semi-char-mode)
    (goto-char (point-max)))
  (keymap-set eat-mode-map "RET" #'pmx--eat-return-to-semi-char)

  (defun pmx--eat-yank-to-semi-char ()
    (interactive)
    (eat-semi-char-mode)
    (yank))
  (keymap-set eat-mode-map "C-y" #'pmx--eat-yank-to-semi-char)

  (defun pmx--eat-yank-from-kill-ring-to-semi-char ()
    (interactive)
    (eat-semi-char-mode)
    (yank-from-kill-ring))
  (keymap-set eat-mode-map
              "M-y" #'pmx--eat-yank-from-kill-ring-to-semi-char)

  (define-obsolete-function-alias 'vterm #'eat "0.1.0"))

(provide 'posimacs-terminal)
;;; posimacs-terminal.el ends here

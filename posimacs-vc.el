;;; posimacs-vc --- Version Control

;;; Commentary:
;;
;; Version control.  Magit basically.  Magit is basically a reason to use Emacs.
;;

;;; Code:

(use-package magit
  :custom
  ;; also try magit-display-buffer-fullframe-status-v1
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  ;; stolen from https://www.manueluberti.eu/emacs/2018/02/17/magit-bury-buffer/
  (defun pk-magit-kill-buffers (param)
    "Restore window configuration and kill all Magit buffers."
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (setq magit-bury-buffer-function #'pk-magit-kill-buffers))

(use-package magit-todos
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))


(use-package git-messenger)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; posimacs-vc.el ends here

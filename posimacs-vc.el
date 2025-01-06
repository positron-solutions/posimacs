;;; posimacs-vc --- Version Control  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Version control.  Magit basically.  Magit is basically a reason to use Emacs.
;;

;;; Code:

(use-package magit
  :ensure (magit :tag latest)
  :custom
  ;; also try magit-display-buffer-fullframe-status-v1
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setopt magit-bury-buffer-function #'magit-mode-quit-window))

(use-package magit-todos
  :after magit
  :hook (magit-mode-hook . magit-todos-mode))

;; TODO learn how to use this lol
;; (use-package git-timemachine
;;   :config)

(use-package ediff
  :ensure nil
  ;; Prot's setup.  These are experimental.
  :config
  (setopt ediff-keep-variants nil)
  ;; (setopt ediff-make-buffers-readonly-at-startup nil)
  ;; (setopt ediff-merge-revisions-with-ancestor t)
  ;; (setopt ediff-show-clashes-only t)
  (setopt ediff-split-window-function 'split-window-horizontally)
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain))

;; TODO ?
(use-package git-messenger)

(use-package git-modes)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;; posimacs-vc.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

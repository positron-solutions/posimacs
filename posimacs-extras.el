;;; posimacs-bindings --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Doom has a nice comprehensive theme system and lots of good starting points.
;; Let's adopt!
;;

;;; Code:

;; when defining bindings and themes via non-custom paths, `restart-emacs` is convenient
(use-package restart-emacs)

(straight-use-package
 '(snow :type git :host github :repo "alphapapa/snow.el"))

(use-package no-littering)

;; tracking command usage to measure pain
(use-package keyfreq
  :init (progn (add-hook 'after-init-hook 'keyfreq-mode t)
               (add-hook 'after-init-hook 'keyfreq-autosave-mode t)))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package sudo-edit) ; upgrade perms to write read-only file


(defun pmx-hack-locals-other-window (target-buffer)
  "Hack elisp with this buffer's buffer locals"
  (interactive (list (read-buffer "Target buffer: "
                            (current-buffer) ; default
                            nil ; require match
                            nil ; filter pred
                            )))
  (let ((new-buffer-name (format "*ielm-<%s>*" target-buffer)))
    (pop-to-buffer (get-buffer-create new-buffer-name))
    (ielm new-buffer-name)
    (ielm-change-working-buffer target-buffer)))

(global-set-key (kbd "M-i") 'pmx-hack-locals-other-window)

;;; posimacs-extras.el ends here

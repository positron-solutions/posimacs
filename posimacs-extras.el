;;; posimacs-bindings --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Packages and functions which are at best fun or useful on rare occasions.

;;; Code:

;; when defining bindings and themes via non-custom paths, `restart-emacs` is convenient
(use-package restart-emacs)

(use-package snow
  :elpaca (snow :host github :repo "alphapapa/snow.el"))

;; tracking command usage to measure pain
(use-package keyfreq
  :init
  (setq keyfreq-file (expand-file-name "etc/keyfreq/keyfreq" user-emacs-directory))
  (setq keyfreq-excluded-commands '(self-insert-command))
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package sudo-edit) ; upgrade perms to write read-only file

;;;###autoload
(defun screenshot-svg ()
  "Save a screenshot of the current frame as an SVG image.
Saves to a temp file and puts the filename in the kill ring."
  (interactive)
  (let* ((filename (make-temp-file "Emacs" nil ".svg"))
         (data (x-export-frames nil 'svg)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

  (interactive)



(provide 'posimacs-extras)
;;; posimacs-extras.el ends here

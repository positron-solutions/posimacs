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

; (use-package no-littering) ; organize ~/.emacs.d

;; tracking command usage to measure pain
(use-package keyfreq
  :init (progn (add-hook 'after-init-hook 'keyfreq-mode t)
               (add-hook 'after-init-hook 'keyfreq-autosave-mode t)))

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


(defun pmx--mru-buffer ()
  "Get the buffer of the most recently used window."
  (window-buffer (get-mru-window t t t)))

(defun pmx--not-ielm-buffer-p (buffer)
  "Filter predicate to remove ielm buffers from `read-buffer' candidates.

BUFFER candidate"
  (not (eq (buffer-local-value 'major-mode buffer)
           'inferior-emacs-lisp-mode)))

(defun pmx-ielm-this-buffer (&optional target-buffer)
  "Hack this buffer's locals with elisp in ielm.

TARGET-BUFFER buffer whose locals we want to hack

If the TARGET-BUFFER is already an ielm buffer, warn instead.
When TARGET-BUFFER is nil, use the result of `current-buffer'"
  (interactive)
  (let* ((target-buffer (or target-buffer (current-buffer)))
        (new-buffer-name (format "*ielm-<%s>*" target-buffer)))
    (if (pmx--not-ielm-buffer-p target-buffer)
      (progn (pop-to-buffer (get-buffer-create new-buffer-name))
             (ielm new-buffer-name)
             (ielm-change-working-buffer target-buffer))
      (user-error "Target buffer %s is already an ielm buffer" (buffer-name target-buffer)))))



;;; posimacs-extras.el ends here

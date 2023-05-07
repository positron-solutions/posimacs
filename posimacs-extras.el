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

;;;###autoload
(defun pmx-mandelbrot ()
  "Render a mandelbrot."
  (interactive)
  (pop-to-buffer (get-buffer-create "*mandelbrot*"))
  (let ((w 800) (h 720) (d 64))
    (fundamental-mode) (erase-buffer)
    (set-buffer-multibyte nil)
    (insert (format "P6\n%d %d\n255\n" w h))
    (dotimes (y h)
      (dotimes (x w)
        (let ((cx (* 1.5 (/ (- x (/ w 1.45)) w 0.45)))
               (cy (* 1.5 (/ (- y (/ h 2.0)) h 0.5)))
               (zr 0)
               (zi 0))
          (let ((v (cl-dotimes (v d d)
                    (if (> (+ (* zr zr) (* zi zi)) 4) (cl-return v)
                      (cl-psetq
                       zr (+ (* zr zr) (- (* zi zi)) cx)
                       zi (+ (* (* zr zi) 2) cy))))))
            (insert-char (floor (* 256 (/ v 1.0 d))) 3)))))
    (image-mode)))



(provide 'posimacs-extras)
;;; posimacs-extras.el ends here

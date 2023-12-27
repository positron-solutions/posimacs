;;; posimacs-extras --- Things are looking up -*- lexical-binding: t -*-

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
  (setq keyfreq-excluded-commands '(self-insert-command org-self-insert-command))
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package sudo-edit) ; upgrade perms to write read-only file

(defun pmx--keypression-ignore (cmd)
  "Ignore bad CMD."
  (or (member cmd '(self-insert-command
                    org-tree-slide-move-next-tree
                    org-tree-slide-move-previous-tree
                    org-self-insert-comand
                    pmx-org-tree-slide-quit
                     ;; counsel-M-x
                    ivy-done))
      ;; todo filter if pre-command, but not post-command
      (string-match-p (rx line-start (literal "special-lispy-"))
                      (symbol-name cmd))
      (string-match-p (rx line-start (literal "lispy-"))
                      (symbol-name cmd))))

(use-package keypression
  :config
  (setopt keypression-combine-same-keystrokes t)
  (setopt keypression-foreground-for-dark-mode "#000000")
  (setopt keypression-background-for-dark-mode "#FFD000")
  ;; TODO roll into youtube setup
  (setopt keypression-y-offset 100)
  (setopt keypression-fade-out-fps 60)
  (setopt keypression-fade-out-delay 1.0)
  (setopt keypression-font-face-attribute
          '(:width normal :height 300 :weight bold))
  (setopt keypression-cast-command-name t)
  (setopt keypression-frames-maxnum 6)
  (setopt keypression-pre-or-post-command 'post-command)
  (setopt keypression-ignored-commands #'pmx--keypression-ignore)
  (setopt keypression-use-child-frame nil) ; child frames have no opacity :-(
  (setopt keypression-concat-self-insert nil)
  (setopt keypression-frame-background-mode 'dark))

;; (use-package keypression
;;   :elpaca (keypression :host github :repo "chuntaro/emacs-keypression"
;;                        :remotes ("positron" :repo "chuntaro/emacs-keypression")))

;;;###Autoload
(defun pmx-screenshot-svg ()
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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

;;; posimacs-extras --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Packages and functions which are at best fun or useful on rare occasions.

;;; Code:

;; when defining bindings and themes via non-custom paths, `restart-emacs` is convenient
(use-package restart-emacs)

(use-package snow
  :ensure (snow :host github :repo "alphapapa/snow.el"))

(use-package mandelbrot
  :ensure (mandelbrot :repo "/home/satoshi/.emacs.d/scratch-pkgs/mandelbrot/"))

;; tracking command usage to measure pain
(use-package keyfreq
  :init
  (setq keyfreq-file (expand-file-name "etc/keyfreq/keyfreq" user-emacs-directory))
  (setq keyfreq-excluded-commands '(self-insert-command org-self-insert-command))
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(use-package vlf ; view large files
  :config (require 'vlf-setup))

(use-package sudo-edit ; upgrade perms to write read-only file
  :commands (sudo-edit
             sudo-edit-find-file))

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

(use-package keycast
  :config
  (setopt keycast-mode-line-format
          "%k%c%r ")
  (setopt keycast-substitute-alist
          '((keycast-log-erase-buffer nil nil)
            (transient-update         nil nil)
            (self-insert-command      nil nil)
            (mwheel-scroll nil nil)))

  ;; The actual integration
  (defun pmx-keycast-doom-modeline-update (return)
    (if (eq (current-buffer) (window-buffer (old-selected-window)))
        (concat return " " keycast-last-formatted)
      return))

  ;; Mode line hooks get called on start and stop :-)
  (defun pmx-keycast-doom-modeline-integrate ()
    (if keycast-freestyle-mode
        (advice-add #'doom-modeline-segment--buffer-position :filter-return
                    #'pmx-keycast-doom-modeline-update)
      (advice-remove #'doom-modeline-segment--buffer-position
                     #'pmx-keycast-doom-modeline-update)))

  (add-hook 'keycast-freestyle-mode-hook #'pmx-keycast-doom-modeline-integrate))

(provide 'posimacs-extras)
;;; posimacs-extras.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

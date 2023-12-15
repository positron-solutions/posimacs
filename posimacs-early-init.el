;;; posimacs-early-init.el --- standard speedups & bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; tricks to speed up loading and avoid unwanted work & initialization that
;; would affect downstream init.

;;; Code:

(setq package-enable-at-startup nil) ; prevent package.el so we can use elpaca

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
;; This is reset in posimacs-init.el after `elpaca-after-init-hook' it is reset
;; to the original value.
(defvar file-name-handler-alist--old file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Note, `gcmh' package will modify this later, so we don't reset it.
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

;; Use the no-littering var directory for elisp native compile cache
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
        (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Turn off graphics features that would slow down initial startup.
;; https://github.com/Gavinok/emacs.d
(setq-default
 package-native-compile t
 inhibit-startup-message t
 mode-line-format nil
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (internal-border-width . 0)
   (undecorated . t)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)
   (use-frame-synchronization . t)))

(provide 'posimacs-early-init)
;;; posimacs-early-init.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

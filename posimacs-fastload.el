;;; posimacs-fastload.el --- standard speedups & bootstrap -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; tricks to speed up loading and basic initialization of the package system &
;; some load paths that are frequently first in an init.el

;;; Code:

(setq package-enable-at-startup nil) ; prevent package.el so we can use elpaca

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defun pmx--reset-gc-threshold ()
  "Revert fast-load settings.
Note, `gcmh' package will modify this at will."
  (setq gc-cons-threshold 100000000 ; 100MB
        file-name-handler-alist file-name-handler-alist-old)
  (garbage-collect))

;; TODO delay gc until after Elpaca queues?
(add-hook 'emacs-startup-hook #'pmx--reset-gc-threshold)

;; Turn off graphics features that would slow down initial startup.
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'posimacs-fastload)
;;; posimacs-fastload.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

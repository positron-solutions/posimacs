;;; posimacs-fastload --- Posimacs

;;; Commentary:
;;
;; tricks to speed up loading and basic initialization of the package system &
;; some load paths that are frequently first in an init.el

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup Optimizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defun reset-gc-threshold ()
  "Set threshold of garbage collector to a better permanent value and invoke garbage colletion now."
  (setq gc-cons-threshold 100000000 ; 100MB
        file-name-handler-alist file-name-handler-alist-old)
  (garbage-collect))

(add-hook 'emacs-startup-hook #'reset-gc-threshold)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Minimalist UI Choices ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t) ; GNU's Not Unix
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1) ; show columns in modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Paths & Packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq package-enable-at-startup nil) ;


;;; posimacs-fastload.el ends here

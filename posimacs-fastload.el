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

;; Early init goes here after version 27
;; https://www.reddit.com/r/emacs/comments/enmbv4/earlyinitel_reduce_init_time_about_02_sec_and/

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; posimacs-fastload.el ends here

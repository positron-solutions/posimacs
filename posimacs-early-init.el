;;; posimacs-early-init.el --- standard speedups & bootstrap -*-lexical-binding: t-*-

;;; Commentary:
;;
;; Tricks to speed up loading and avoid unwanted work & initialization that
;; would affect downstream init.
;;
;; Don't draw things on startup that would be turned off later (toobars etc).
;;
;; Remove graphical flickers and set up some colors that will mesh better with
;; the actual theme loading or at least not blind us when the config has a bug.
;;
;; Variables that are particular to Emacs being installed via Nix.

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
      gc-cons-percentage 2.0
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
 frames-inhibit-implied-resize t        ; please don't mess up screencasts
 mode-line-format nil
 ;; Builds with Nix set this to a garbage time.  Elpaca needs to guess if
 ;; built-in packages are out of date or not.
 emacs-build-time '(26026 11222 728074 741000)
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (internal-border-width . 0)
   (undecorated . t)
   (background-color . "#000000")      ; said to reduce startup flicker on theme
   (ns-appearance . dark)              ; darwin
   (ns-transparent-titlebar . t)       ; darwin
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)
   (use-frame-synchronization . t)))
(set-face-attribute 'default nil :foreground "#CCCCCC") ; dark background, light text

(provide 'posimacs-early-init)
;;; posimacs-early-init.el ends here

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

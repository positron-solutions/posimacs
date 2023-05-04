;;; init.el --- You know -*- lexical-binding: t -*-
;;; Commentary:
;; Load the code
;;
;; Load only parts of it if you must.

(defvar native-comp-deferred-compilation-deny-list nil)

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
(setq straight-disable-native-compile nil)

;; Load no-littering just after boostrapping straight.  Otherwise, packages may
;; initialize to weird locations.
(use-package no-littering
  :init
  (require 'no-littering))


;; Needs to load early for some reason
(straight-use-package 'org)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; idle garbage collection
(use-package gcmh
  :demand t
  :config
  (add-hook 'emacs-startup-hook (lambda () (gcmh-mode 1))))

;;; Code:
(let ((posimacs-files '("posimacs-bindings.el"
                        "posimacs-defaults.el"
                        "posimacs-elisp.el"
                        "posimacs-extras.el"
                        "posimacs-minibuffer.el"
                        "posimacs-prog.el"
                        "posimacs-style.el"
                        "posimacs-vc.el"

                        "jinx/posimacs-jinx.el"
                        "rust/posimacs-rust.el"
                        "vterm/posimacs-terminal.el")))
  (dolist (file-name posimacs-files)
    (load (expand-file-name
           (concat "posimacs/" file-name)
           user-emacs-directory))))

;;; posimacs-init.el ends here.

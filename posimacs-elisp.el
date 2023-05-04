;;; posimacs-elisp --- Making elisp hacking better out of the box -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A completion framework and sources of suggestions to populate it are the most
;; essential behavior to enable in Emacs.  It makes discovering the rest of
;; Emacs as easy as searching, frequently adding additional context to the
;; results such as displaying hotkeys or docstrings, making all workflows become
;; extremely fast with less time spent reading documentation.
;;

;;; Code:

(use-package ielm
  :config
  ;; When the output is huge, ielm can slow down.  Instead, let's just get rid
  ;; of large output since there are other ways to handle this better.
  (setq-local comint-max-line-length 1024)
  (declare-function comint-truncate-buffer "comint.el")
  (unless (memq #'comint-truncate-buffer comint-output-filter-functions)
    (push #'comint-truncate-buffer comint-output-filter-functions)))

;;; posimacs-elisp.el ends here

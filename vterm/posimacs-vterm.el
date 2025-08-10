;;; posimacs-vterm.el --- Do normie things in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Only needs to set the load path to include the out dir we defined in the
;; emacs-vterm home manager module.

;;; Code:

(use-package vterm
  :elpaca nil
  :load-path  "~/.emacs.d/vendor/emacs-vterm"
  :commands (vterm vterm-other-window))

(provide 'posimacs-vterm)
;;; posimacs-vterm.el ends here

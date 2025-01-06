;;; posimacs-cl.el --- Posimacs Common Lisp  -*- lexical-binding: t; -*-

(use-package sly
  :config
  (setq inferior-lisp-program "sbcl")
  ;; TODO set this correctly
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  )

;; TODO Elisp CL interop.
;; (use-package glue)

(provide 'posimacs-cl)
;;; posimacs-cl.el ends here

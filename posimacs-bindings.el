;;; posimacs-bindings --- Default UI of positivity

;;; Commentary:
;;
;; We replace a lot of bindings with more discoverable transient variants that
;; require fewer modifier keys and emphasize the most easily reached modifiers.
;;

;;; Code:

(use-package transient
  :config
  (transient-define-prefix posimacs-help-transient ()
    "A combination of the actually useful help commands "
    ["Help Commands\n"
     ["Describe"
      ("s" "Symbol at point" counsel-describe-symbol)
      ("c" "Command" helpful-command)
      ("k" "Key" helpful-key)
      ("v" "Variable" counsel-describe-variable)
      ("r" "Macro" helpful-macro)
      ("f" "Function" counsel-describe-function)
      ]
     ["Watch"
      ;; XXX Add command log mode back in
      ("l" "Recent commands" view-lossage)
      ("e" "Echo Messages" view-echo-area-messages)
      ]
     ["Search"
      ("a" "Symbol" counsel-apropos)
      ("d" "Documentation" apropos-documentation)
      ("x" "Commands" counsel-M-x)
      ]
     ["Built-in Help"
      ("m" "Emacs manual" info-emacs-manual)
      ("g" "Glossary" search-emacs-glossary)
      ]
     ]
    )
  ;; XXX evaluate / adopt general
  (global-set-key (kbd "M-h") 'posimacs-help-transient))

;;; posimacs-bindings.el ends here

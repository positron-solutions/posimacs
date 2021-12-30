;;; posimacs-bindings --- Default UI of positivity

;;; Commentary:
;;
;; We replace a lot of bindings with more discoverable transient variants that
;; require fewer modifier keys and emphasize the most easily reached modifiers.
;;

;;; Code:

(use-package transient
  :after (general)
  :config
  (transient-define-prefix posimacs-help-transient ()
    "A combination of the actually useful help commands "
    ["Help Commands\n"
     ["Describe"
      ("s" "Symbol at point" helpful-at-point)
      ("c" "Command" helpful-command)
      ("k" "Key" helpful-key)
      ("v" "Variable" counsel-describe-variable)
      ("r" "Macro" helpful-macro)
      ("f" "Function" counsel-describe-function)
      ]
     ["Watch"
      ("l" "Recent commands" clm-toggle)
      ("e" "Echo Messages" view-echo-area-messages)
      ]
     ["Search"
      ("a" "Symbol" counsel-apropos)
      ("d" "Documentation" apropos-documentation)
      ("x" "Commands" counsel-M-x)
      ("b" "Active Bindings" counsel-descbinds)
      ]
     ["Built-in Help"
      ("m" "Emacs manual" info-emacs-manual)
      ("g" "Glossary" search-emacs-glossary)
      ]
     ]
    )
  ;; XXX evaluate / adopt general
  (global-set-key (kbd "M-h") 'posimacs-help-transient))
  (general-unbind 'org-mode-map "M-h")
(use-package general)

;;; posimacs-bindings.el ends here

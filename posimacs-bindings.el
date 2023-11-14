;;; posimacs-bindings.el --- A poorly contrived package -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Positron Solutions <contact@positron.solutions>

;; Author: Positron Solutions <contact@positron.solutions>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/positron-solutions/posimacs

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This module is horrible because it was not in use for a long while.  There's
;; 2-3 modules in development and this is just some hacking line noise that made
;; it into git history to facilitate a quick machine move at some point.
;;
;; ⚠️ Do not use this elisp.  Some of the pmx calls are however useful.

;;; Code:

(defun pmx-toggle-tail-dribble ()
  "Open a dribble file and tail the contents."
  (interactive)
  (let* ((dribble-buffer "*Dribble*")
         (buffer (get-buffer dribble-buffer)))
    (if buffer
        (kill-buffer buffer)
      (with-current-buffer (get-buffer-create dribble-buffer)
        (let ((dribble-file (make-temp-file "dribble")))
          (open-dribble-file dribble-file)
          (insert-file-contents dribble-file 'visit)
          (setq-local dribble-file dribble-file)
          (add-hook 'kill-buffer-hook (lambda () (delete-file dribble-file))
                    nil 'local)
          (auto-revert-tail-mode)
          (setq-local auto-revert-verbose nil
                      buffer-read-only t))
        (switch-to-buffer-other-window dribble-buffer)))))

(use-package general)

;; TODO this is such garbage
(use-package transient
  :after (general avy helpful command-log info help-fns)
  :config
  (transient-define-prefix posimacs-help-transient ()
    "A combination of the actually useful help commands "
    ["Help Commands\n"
     ["Describe"
      ("s" "Symbol at point" helpful-at-point)
      ("v" "Variable" counsel-describe-variable)
      ("f" "Function" counsel-describe-function)
      ("c" "Command" helpful-command)
      ("r" "Macro" helpful-macro)]

     ["Describe Buffer"
      ("k" "Key" helpful-key)
      ("m" "Mode" describe-mode)
      ("b" "Active Bindings" counsel-descbinds)]

     ["Watch"
      ("wc" "Commands" command-log-toggle)
      ("wm" "Messages" view-echo-area-messages)
      ("wd" "Dribble (raw inputs)" pmx-toggle-tail-dribble)
      ;; TODO this is not a watch.  CLM needs to do this, and better.
      ("wC" "Recent Commands" view-lossage)]

     ["Search"
      ("a" "Docstrings ☻" counsel-apropos)
      ;; There's a bug when using this with prefix arg due to a non-string being
      ;; passed in a docstring position.
      ("A" "Documentation" apropos-documentation)
      ("e" "Shortdoc elisp ☻" shortdoc-display-group)
      ("L" "Elisp manual ☻" elisp-index-search)
      ("I" "Info manuals menu ☻" info-display-manual)
      ("C-m m" "Garbage (Emacs manual)" info-emacs-manual)
      ("g" "Glossary ☻" search-emacs-glossary)]])


  ;; TODO forward/-backward-par are usually not a good solution, frequently
  ;; abused for scrolling or where imenu works well.
  (general-unbind "C-<up>") ; backward-paragraph
  (general-unbind "C-<down>") ; forward-paragraph
  (general-def 'global-map "M-p" #'backward-paragraph)
  (general-def 'global-map "M-n" #'forward-paragraph)


  (general-unbind 'ivy-minibuffer-map "M-o")

  (general-def "M-h" 'posimacs-help-transient)
  (general-def "M-j" 'avy-goto-word-1)

  ;; Prefer kill to bury...
  (general-def 'helpful-mode-map  "q" 'kill-this-buffer)
  (general-def 'help-mode-map  "q" 'kill-this-buffer)
  (general-def 'Info-mode-map  "q" 'kill-this-buffer)
  (general-def 'dired-mode-map  "q" 'kill-this-buffer)
  (general-def 'shortdoc-mode-map "q" 'kill-this-buffer)

  ;;; Minor mode maps that bind our priority keys

  (general-unbind 'dired-mode-map "M-o") ; other window
  (general-unbind 'ibuffer-mode-map "M-o")

  (general-unbind 'org-mode-map "M-h")
  (general-unbind 'ibuffer-mode-map "M-s") ; swiper
  (general-unbind 'ibuffer-mode-map "M-j") ; avy

  (general-unbind 'winner-mode-map "C-c <left>") ; winner-undo
  (general-unbind 'winner-mode-map "C-c <right>") ; winner-redo

  (general-unbind "C-<left>") ; previous-buffer
  (general-unbind "C-<right>")) ; next-buffer

(provide 'posimacs-bindings)
;;; posimacs-bindings ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

;;; posimacs-bindings.el --- An orphan struggling to make it -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Positron Solutions <contact@positron.solutions>

;; Author: Positron Solutions <contact@positron.solutions>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/positron-solutions/posimacs

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This module is almost useful.  It should be broken out into a package or
;; several.  User-keys supersedes some unbindings, especially "stupid sequences".

;;; Code:

(defcustom pmx-reverse-ring-size 16
  "How many reverse actions to store."
  :type 'integer
  :group 'pmx)

(defvar-local pmx-reverse--moves nil
  "For use with pmx-reverse.")

(defun pmx-reverse--moves ()
  (or pmx-reverse--moves
      (setq pmx-reverse--moves
            (make-ring pmx-reverse-ring-size))))

(defun pmx-reverse-move ()
  "Reverse the last move."
  (interactive)
  (let ((ring (pmx-reverse--moves)))
    (if-let ((last (and (not (ring-empty-p ring))
                        (ring-remove ring))))
        (goto-char last)
      (user-error "No more reverses available"))))

(defun pmx-reverse-push (reverse)
  (ring-insert-at-beginning (pmx-reverse--moves) reverse))

;; TODO undo char needs a binding
(defun pmx-forward-to-char (char)
  "Just take me to the next char.
Repeats of that char should continue."
  (interactive (list (read-char)))
  (if-let ((found (save-excursion
                    (forward-char)
                    (search-forward (char-to-string char) (window-end) t))))
      (let ((repeat-map (make-sparse-keymap)))
        (keymap-set repeat-map "C-/" #'pmx-reverse-move)
        (keymap-set repeat-map (char-to-string char)
                    (lambda ()
                      (interactive)
                      (pmx-forward-to-char char)))
        (set-transient-map repeat-map t)
        (pmx-reverse-push (point))
        (goto-char (1- found)))
    (backward-char)
    (user-error "Window has no next char: %c" char)))

(defun pmx-backward-to-char (char)
  "Just take me to the previous char.
Repeats of that char should continue."
  (interactive (list (read-char)))
  (if-let ((found (save-excursion
                    (search-backward (char-to-string char) (window-start) t))))
      (let ((repeat-map (make-sparse-keymap)))
        (keymap-set repeat-map "C-/" #'pmx-reverse-move)
        (keymap-set repeat-map (char-to-string char)
                    (lambda ()
                      (interactive)
                      (pmx-backward-to-char char)))
        (set-transient-map repeat-map t)
        (pmx-reverse-push (point))
        (goto-char found))
    (user-error "Window has no previous char: %c" char)))

;;; Repeat maps...
;;
;; Repeat maps only allow one repeat map per command, which means we don't have
;; situational repeats.  This kind of sucks.
;;
;; https://lists.gnu.org/archive/html/emacs-devel/2021-09/msg00679.html
;;
;; (defalias 'forward-word-with-case 'forward-word
;;   "Alias for `forward-word' for use in `case-repeat-map'.")
;; (defalias 'backward-word-with-case 'backward-word
;;   "Alias for `backward-word for use in `case-repeat-map'.")

;; (defvar case-repeat-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "c" #'capitalize-word)
;;     (define-key map "u" #'upcase-word)
;;     (define-key map "l" #'downcase-word)
;;     ;; movement
;;     (define-key map "f" #'forward-word-with-case)
;;     (define-key map "b" #'backward-word-with-case)
;;     map)
;;   "A map to repeat word-casing commands.  For use with `repeat-mode'.")

;; (dolist (command '(capitalize-word
;;                    capitalize-dwim
;;                    upcase-word
;;                    upcase-dwim
;;                    downcase-word
;;                    downcase-dwim
;;                    forward-word-with-case
;;                    backward-word-with-case))
;;   (put command 'repeat-map 'case-repeat-map))

;; TODO move to early loading?
(use-package general
  :config
  ;; Prefer kill to bury...
  (general-def 'helpful-mode-map "q" #'kill-this-buffer)
  (general-def 'help-mode-map "q" #'kill-this-buffer)
  (general-def 'Info-mode-map "q" #'kill-this-buffer)
  (general-def 'dired-mode-map "q" #'kill-this-buffer)
  (general-def 'shortdoc-mode-map "q" #'kill-this-buffer)

  ;; Create some space.  Purge any top-level commands that are not that useful
  ;; compared to others, duplicated with a better binding, or should be
  ;; re-implemented with a better command or sequence of commands.
  (general-unbind 'global-map
    "C-@"                               ; duplicate set-mark-command
    "C-]"                               ; abort-recursive-edit
    "C-_"                               ; undo

    "C-j"                               ; electric-newline-and-maybe-indent
    "C-l"                               ; recenter-top-bottom
    "C-o"                               ; open-line
    "C-r"                               ; isearch-backward
    "C-s"                               ; isearch-forward
    "C-t"                               ; transpose-chars
    "C-u"                               ; universal argument

    "C-<up>"                            ; backward-paragraph
    "C-<down>")                         ; forward-paragraph

  (general-unbind                       ; M-Meta sequences
    'esc-map
    "a"                                 ; backward-sentence
    "b"                                 ; backward-word
    "c"                                 ; capitalize-word
    "d"                                 ; kill-word
    "f"                                 ; forward-word
    "k"                                 ; kill-sentence
    "l"                                 ; downcase
    "m"                                 ; back-to-indentation
    "r"                                 ; move-to-window-line-top-bottom
    "t"                                 ; transpose-words
    "u"                                 ; upcase
    "z")                                ; zap-to-char


  (general-unbind 'ctl-x-map
    ;; TODO warns
    "C-@"                               ; pop-global-mark
    "$"                                 ; set-selective-display
    "+"                                 ; balance-windows
    "."                                 ; set-fill-prefix
    ";"                                 ; comment-set-column

    "C-o"                               ; set-goal-column
    "C-l"                               ; downcase region
    "C-u"                               ; upcase region

    "f"                                 ; set-fill-column
    "l"                                 ; count-lines-page
    "u"                                 ; undo
    ;; TODO there's a lot more cruft
    ;; in the C-x map :-( `user-keys'
    )

  ;; Preen the help map
  (general-unbind 'help-mode-map
    "C-h C-\\"                          ; duplicate describe-input-method
    "C-h C-a"                           ; about-emacs
    "C-h C-c"                           ; describe-copying
    "C-h C-e"                           ; view-external-packages
    "C-h C-o"                           ; describe-distribution
    "C-h C-p"                           ; finder-by-keyword
    "C-h C-s"                           ; search-forward-help-for-help
    "C-h C-w"                           ; describe-no-warranty
    "C-h RET"                           ; view-order-manuals
    "C-h g"                             ; describe-gnu-project
    "C-h h"                             ; view-hello-file
    "C-h t")                            ; help-with-tutorial

  ;; Replacement definitions, more coherence in defaults
  (general-unbind 'global-map "C-v")
  (general-def 'global-map "M-c" #'scroll-down-command)
  (general-def 'global-map "M-v" #'scroll-up-command)
  (general-def 'global-map "M-r" #'repeat)

  ;; And now for bindings I actually want
  (general-def 'global-map "M-e" #'eval-expression)
  (general-def 'org-mode-map "M-e" #'eval-last-sexp)
  (general-def 'dired-mode-map "j" #'dired-jump)

  (general-def 'global-map "M-j" #'pmx-backward-to-char)
  (general-def 'global-map "M-k" #'pmx-forward-to-char)
  (general-def 'global-map "M-l" #'avy-goto-word-1)

  (defun pmx-backward-symbol ()
    "Backward symbol should exist."
    (interactive)
    (forward-symbol -1))

  (general-def 'global-map "M-f" #'forward-symbol)
  (general-def 'global-map "M-b" #'pmx-backward-symbol)

  ;; TODO add to mode maps for regex input
  ;; C-q is borderline M-x, but useful for regex input
  ;; TODO finder-by-keyword
  ;; TODO `user-keys'
  ;; TODO C-u and other window commands

  ;; If you need to break a bad habit with not using `ibuffer'
  ;; and `switch-to-buffer'.
  ;; (general-unbind "C-<left>")   ; previous-buffer
  ;; (general-unbind "C-<right>")  ; next-buffer

  ;; Remove unfortunate shadows in modes where the keymaps were written by
  ;; spammers
  (general-unbind 'dired-mode-map "M-s")
  (general-unbind 'ivy-minibuffer-map "M-o")
  (general-unbind 'dired-mode-map "M-o")
  (general-unbind 'ibuffer-mode-map "M-o")
  (general-unbind 'org-mode-map "M-h")
  (general-unbind 'org-mode-map "M-j")
  (general-unbind 'org-mode-map "M-o")
  (general-unbind 'org-mode-map "M-h")
  (general-unbind 'org-mode-map "M-e")
  (general-unbind 'ibuffer-mode-map "M-s")
  (general-unbind 'ibuffer-mode-map "M-j")
  (general-unbind 'lispy-mode-map "M-k")

  ;; Finish up by correcting repeat maps that were screwd up
  (setq undo-repeat-map (make-sparse-keymap))
  (put #'undo 'repeat-map undo-repeat-map)
  (keymap-set undo-repeat-map "/" #'undo))

;; TODO new package for sure
(use-package transient
  :after (general avy command-log info help-fns)
  :elpaca (transient :tag latest)
  :config

  (defun pmx-watch-variable ()
    "Watch variable changes with a generic watch callback."
    (interactive)
    ;; TODO implement this
    (undefined))

  (transient-define-prefix pmx-debugging ()
    "Debugger states & controls"
    [["Debug"
      ("t" "toggle debug-on-error" toggle-debug-on-error)
      ("q" "toggle debug-on-quit" toggle-debug-on-quit)
      ("e" "debug on entry" debug-on-entry)
      ("v" "debug variable change" debug-on-variable-change)
      ("w" "watch variable changes" pmx-watch-variable)]
     ["Edebug"
      ;; TODO display states
      ("e" "instrument defun" edebug-defun)
      ("r" "remove instrumentation" edebug-remove-instrumentation)
      ""
      ;; TODO display states
      ("C-f" "all forms" edebug-all-forms)
      ("C-d" "all defs" edebug-all-defs)]
     ;; TODO tracing
     ["Profiler"
      ("s" "start" profiler-start)
      ;; TODO this is loaded after start
      ("S" "stop" profiler-stop)
      ("R" "report" profiler-report)]])

  (defun pmx-info-elisp-goto ()
    "Go to the Elisp manual and search nodes."
    (interactive)
    (info-display-manual "elisp")
    (call-interactively #'Info-goto-node))

  (defun pmx-info-elisp-exact-at-point ()
    "Go to the Elisp manual and search exact."
    (interactive)
    (if-let* ((symbol (symbol-at-point))
              (name (symbol-name symbol)))
        (progn
          (info-display-manual "elisp")
          (set-text-properties 0 (length name) nil name)
          (when  (Info-search name)
            (recenter)))
      (progn
        (info-display-manual "elisp")
        (call-interactively #'Info-search))))

  (transient-define-prefix pmx-help ()
    "A combination of the actually useful help commands "
    ["Help Commands\n"
     ["Elisp"
      ("s" "Symbol at point" helpful-at-point)
      ("v" "Variable" counsel-describe-variable)
      ("f" "Function" counsel-describe-function)
      ("c" "Command" helpful-command)
      ("r" "Macro" helpful-macro)
      ("a" "Symbols" counsel-apropos)
      ""
      ("R" "Regexp Builder" regexp-builder)]

     ["Debug"
      ("d" pxm-debugging)]

     ["Bindings"
      ;; TODO whereis
      ("k" "Key" helpful-key)
      ("m" "Mode" describe-mode)
      ("b" "Active Bindings" counsel-descbinds)]

     ;; TODO Helpful could use some upstream for text properties, faces,
     ;; characters.  The outputs are a bit difficult to parse at first.
     ;; Recreating the states is not intuitive.
     ["Text"
      ("df" "Describe Face" describe-face)
      ("dc" "Describe Char" describe-char)
      ("dp" "Describe Text Properties" describe-text-properties)]

     ["Watch"
      ;; TODO keycast!
      ("wc" "Commands" command-log-toggle)
      ;; TODO updates with 'g'.. guess they will learn
      ("wC" "Command History (static)" view-lossage)
      ("wm" "Messages" view-echo-area-messages)
      ("wd" "Dribble (raw inputs)" command-log-tail-dribble)
      ]

     ["Docs"
      ;; There's a bug when using this with prefix arg due to a non-string being
      ;; passed in a docstring position.
      ("A" "Documentation" apropos-documentation)
      ("e" "Elisp Short Doc" shortdoc-display-group)
      ("L" "Elisp manual" pmx-info-elisp-goto)
      ("S" "Elisp search" pmx-info-elisp-exact-at-point)
      ("I" "All manuals" info-display-manual)
      ("C-m m" "Emacs Manual" info-emacs-manual)
      ("g" "Glossary" search-emacs-glossary)]])

  (general-def "M-h" 'posimacs-help-transient))

(provide 'posimacs-bindings)
;;; posimacs-bindings ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

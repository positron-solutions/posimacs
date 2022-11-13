;;; posimacs-bindings --- Default UI of positivity

;;; Commentary:
;;
;; We replace a lot of bindings with more discoverable transient variants that
;; require fewer modifier keys and emphasize the most easily reached modifiers.
;;
;; This module is horrible because it was not in use for a long while.  There's
;; 2-3 modules in development and this is just some hacking line noise that made
;; it into git history to facilitate a quick machine move at some point.
;;
;; Do not use this elisp.  Some of the pmx calls are however useful.
;;

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
          (add-hook 'kill-buffer-hook (lambda () (delete-file dribble-file))
                    nil 'local)
          (auto-revert-tail-mode)
          (setq-local auto-revert-verbose nil
                      buffer-read-only t))
        (switch-to-buffer-other-window dribble-buffer)))))

(use-package general)

(use-package transient
  :after (general)
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
      ("wc" "Commands" clm-toggle)
      ("wm" "Messages" view-echo-area-messages)
      ("wd" "Dribble (raw inputs)" pmx-toggle-tail-dribble)
      ;; TODO this is not a watch.  CLM needs to do this, and better.
      ("wC" "Recent Commands" view-lossage)]

     ["Search"
      ("a" "Docstrings" counsel-apropos)
      ("A" "Documentation" apropos-documentation)
      ("e" "Shortdoc elisp" shortdoc-display-group)
      ("L" "Elisp manual (good)" elisp-index-search)
      ("M" "Emacs manual (bad)" info-emacs-manual)
      ("g" "Glossary" search-emacs-glossary)]])

  ;;; Thanks glucas
  ;; https://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function
  (defun pmx--switch-to-last-window ()
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

  (defun pmx-switch-to-last-window ()
    "Switch to previous window"
    (interactive)
    (pmx--switch-to-last-window))

  (defun pmx-switch-window ()
    "Other window.  On repeat, window controls"
    (interactive)
    (if (string= (symbol-name last-command) "pmx-switch-window")
        (progn (if (<= (count-windows) 2) (pmx--switch-to-last-window)
                 (switch-window)))
      (pmx-switch-to-last-window)))

  (general-unbind 'org-mode-map "M-h")

  (general-unbind "M-j")
  (general-unbind "M-a")
  (general-unbind "M-m")
  (general-def "M-a" 'back-to-indentation)

  (general-unbind "C-v") ; scroll-up-command
  (general-unbind "M-v") ; scroll-down-command
  (general-unbind "M-c") ; capitalize-word

  (general-unbind "C-<up>") ; backward-paragraph
  (general-unbind "C-<down>") ; forward-paragraph
  (general-def 'global-map "M-p" #'backward-paragraph)
  (general-def 'global-map "M-n" #'forward-paragraph)

  ;; Allow best keys in vterm
  (general-unbind 'vterm-mode-map "M-a")
  (general-unbind 'vterm-mode-map "M-s")
  (general-unbind 'vterm-mode-map "M-d")
  (general-unbind 'vterm-mode-map "M-f")
  (general-unbind 'vterm-mode-map "M-h")
  (general-unbind 'vterm-mode-map "M-j")
  (general-unbind 'vterm-mode-map "M-k")


  (defun pmx-vterm-avy-goto-word-1 ()
    "Jump, but enable vterm copy mode first"
    (interactive)
    (unless vterm-copy-mode
      (vterm-copy-mode 1))
    (call-interactively 'avy-goto-word-1))

  (defun pmx-vterm-kill-region ()
    "Kill, but enable vterm copy mode first"
    (interactive)
    (unless vterm-copy-mode
      (vterm-copy-mode 1))
    (call-interactively 'kill-region))

  (defun pmx-vterm-send-enter ()
    "Enter, but disable vterm copy mode first"
    (interactive)
    (when vterm-copy-mode
      (vterm-copy-mode -1))
    (call-interactively 'vterm-send-enter))

  (defun pmx-vterm-yank ()
    "Yank, but disable vterm copy mode first"
    (interactive)
    (when vterm-copy-mode
      (vterm-copy-mode -1))
    (call-interactively 'vterm-yank-primary))

  (general-def 'vterm-mode-map "M-j" 'pmx-vterm-avy-goto-word-1)
  (general-def 'vterm-mode-map "M-w" 'pmx-vterm-kill-region)
  (general-def 'vterm-mode-map "RET" 'pmx-vterm-send-enter)
  (general-def 'vterm-mode-map "C-y" 'vterm-yank)

  ;; detect if minibuffer is open or not to avoid closing buffers
  ;; (general-def "M-g" 'keyboard-escape-quit)
  ;; (general-unbind "C-g") ; keyboard-quit

  (defun pmx-keyboard-quit ()
    "Quit, but if minibuffer is open and not focused, quit it."
    (interactive)
    (declare-function minibuffer-keyboard-quit "delsel" ())
    (if (active-minibuffer-window)
        (progn (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
               (select-window (active-minibuffer-window))
               (minibuffer-keyboard-quit))
      (keyboard-quit)))

  (general-def 'global-map "C-g" 'pmx-keyboard-quit)

  ;; Using M-g everywhere instead of C-g
  (define-key key-translation-map (kbd "M-g") (kbd "C-g"))

  (general-def 'helpful-mode-map  "q" 'kill-this-buffer)
  (general-def 'help-mode-map  "q" 'kill-this-buffer)
  (general-def 'Info-mode-map  "q" 'kill-this-buffer)
  (general-def 'dired-mode-map  "q" 'kill-this-buffer)
  (general-def 'shortdoc-mode-map "q" 'kill-this-buffer)

  ;; Modes that interfere with other-window
  (general-unbind 'dired-mode-map "M-o")
  (general-unbind 'ibuffer-mode-map "M-o")
  (general-unbind 'ibuffer-mode-map "M-s")

  (general-unbind 'ibuffer-mode-map "M-j")

  (general-unbind "C-x o") ; other window
  (general-unbind "C-x d") ; dired
  (general-unbind "C-x f") ; counsel-find-file

  (general-unbind "C-s") ; isearch

  ;; Unbinding commands that are:
  ;;   1) scattered
  ;;   2) have been gathered into transients of related commands
  ;;   3) don't fit the RISC principle

  (general-unbind "C-x M-g") ; magit-dispatch
  (general-unbind "C-x g") ; magit-status

  (general-unbind "C-x 0") ; delete-window
  (general-unbind "C-x 1") ; delete other windows
  (general-unbind "C-x 2") ; split window below
  (general-unbind "C-x 3") ; split window right
  (general-unbind "C-x 4") ; prefix for other-window
  (general-unbind "C-x 5") ; prefix for other-frame
  (general-unbind "C-x {") ; shrink-window-horizontally
  (general-unbind "C-x }") ; enlarge-window-horizontally
  (general-unbind "C-x +") ; balance windows
  (general-unbind "C-x -") ; shrink-window-if-larger-than-buffer

  (general-unbind "C-x C-b") ; ibuffer
  (general-unbind "C-x h") ; mark entire buffer
  (general-unbind "C-x C-p") ; mark page (seems broken?)
  (general-unbind "C-x TAB") ; indent-rigidly
  (general-unbind "C-x C-SPC") ; pop-global-mark
  (general-unbind "C-x SPC") ; rectangle mode

  (general-unbind "C-x [") ; backward-page
  (general-unbind "C-x ]") ; forward-page
  (general-unbind "C-x <") ; scroll-left
  (general-unbind "C-x >") ; scroll-right


  (general-unbind "C-x i") ; insert-file
  (general-unbind "C-x =") ; what-cursor-position
  (general-unbind "C-x m") ; compose-mail
  (general-unbind "C-x X") ; edebug prefix
  ;; ever wanted to press shift without using shift key?
  (general-unbind "C-x @") ; event-apply
  (general-unbind "C-x *") ; calc-dispatch
  (general-unbind "C-x C-d") ; list-directory
  (general-unbind "C-x v") ; vc-prefix-map

  (general-unbind "C-x t") ; transpose-lines
  (general-unbind "C-x C-x") ; exchange-point-and-mark
  (general-unbind "C-<left>") ; previous-buffer
  (general-unbind "C-<right>") ; next-buffer

  ;; TODO combine comment line / region
  (general-unbind "C-x C-;") ; comment-line

  ;; this binding is horrific
  (general-unbind "C-x M-:") ; repeat-complex-command

  (general-unbind "C-x n") ; narrow prefix
  (general-unbind "C-x =") ; text-scale-adjust
  (general-unbind "C-x C-u") ; upcase-region
  (general-unbind "C-x C-l") ; downcase-region
  (general-unbind "C-x C-a") ; edebug prefix

  (general-unbind "C-x a") ; abbrev prefix

  (general-unbind 'flycheck-mode-map "C-c !") ; flycheck prefix
  (general-unbind 'yas-minor-mode-map "C-c &") ; yasnippet prefix
  (general-unbind "C-c o") ; toggle command-line-mode buffer display
  (general-unbind "C-c M-g") ; magit-dispatch-file
  (general-unbind 'winner-mode-map "C-c <left>") ; winner-undo
  (general-unbind 'winner-mode-map "C-c <right>") ; winner-redo
  (general-unbind "C-c ESC") ; idk but it's listed

  ;; set-selective-display and other narrowing controls need help

  ;; Redundant
  (general-unbind "C-t")
  (general-unbind "<f1>")
  (general-unbind "<f2>")
  (general-unbind "<f3>")
  (general-unbind "<f4>")

  (setq stupid-keys
        '("home"
          "insert"
          "insertchar"
          "end"
          "up"
          "begin"
          "prior"
          "right"
          "find"
          "execute"
          "<f13>"
          "<f14>"
          "<f15>"
          "<f16>"
          "<f17>"
          "<f18>"
          "<f19>"
          "<f20>"
          "!"
          "@"
          "#"
          "$"
          "%"
          "^"
          "&"
          "*"
          "("
          ")"
          "{"
          "}"
          "`"
          "~"
          ":"
          "\""
          "<"
          ">"
          "?"
          "|"))

  (dolist (modifier '("M" "S" "C" "C-M" "C-S"))
    (dolist (stupid-key stupid-keys)
      (let ((key (message "killed %s" (format "%s-%s" modifier stupid-key))))
        (general-unbind))))

  ;; F10 & F11 window manager behaviors handled by Emacs itself

  ;; elisp
  (general-unbind "C-M-SPC") ; mark-sexp

  ;; digit arguments can be done with universal arg
  (general-unbind
    "C-M-1"
    "C-M-2"
    "C-M-3"
    "C-M-4"
    "C-M-5"
    "C-M-6"
    "C-M-7"
    "C-M-8"
    "C-M-9")

  (general-def "M-h" 'posimacs-help-transient)
  (general-def "M-o" 'pmx-switch-window)
  (general-def "M-j" 'avy-goto-word-1))

;;; posimacs-bindings.el ends here

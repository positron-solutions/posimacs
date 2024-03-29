;;; posimacs-bindings --- Default UI of positivity

;;; Commentary:
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

(defun pmx-keyboard-quit ()
  "Quit, but if minibuffer is open and not focused, quit it."
  (interactive)
  (declare-function minibuffer-keyboard-quit "delsel" ())
  (if (active-minibuffer-window)
      (progn (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
             (select-window (active-minibuffer-window))
             (minibuffer-keyboard-quit))
    (keyboard-quit)))

(use-package general)

(use-package transient
  :after (general vterm avy helpful)
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

  ;; Vterm's terminal bindings interfere with even basic text editing ;-(
  ;; use `vterm-send-C-S-a' etc.
  (general-unbind 'vterm-mode-map
    "M-a"
    "M-s"
    "M-d"
    "M-f"
    "M-h"
    "M-j"
    "M-k")

  ;; Rebind vterm with modified shortcuts that enable `vterm-copy-mode'
  (general-def 'vterm-mode-map "M-j" 'pmx-vterm-avy-goto-word-1)
  (general-def 'vterm-mode-map "M-w" 'pmx-vterm-kill-region)
  (general-def 'vterm-mode-map "RET" 'pmx-vterm-send-enter)
  (general-def 'vterm-mode-map "C-y" 'vterm-yank)

  (general-def "M-h" 'posimacs-help-transient)
  (general-def "M-o" 'pmx-switch-window)
  (general-def "M-j" 'avy-goto-word-1)

  ;; Navigate to minibuffer before executing quit if necessary
  (general-def 'global-map "C-g" 'pmx-keyboard-quit)

  ;; Using M-g everywhere instead of C-g
  (define-key key-translation-map (kbd "M-g") (kbd "C-g"))

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
;;; posimacs-bindings.el ends here

;;; posimacs-bindings --- Default UI of positivity

;;; Commentary:
;;
;; We replace a lot of bindings with more discoverable transient variants that
;; require fewer modifier keys and emphasize the most easily reached modifiers.
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

  (transient-define-prefix pmx-projectile ()
    "Projectile transient UI"
    ["Projectile\n"
     ["All Projects"
      ("F" "Find known file" projectile-find-file-in-known-projects)
      ("J" "Save projectile projects" projectile-save-known-projects)
      ]
     ["Project"
      ("y" "Dirty projects" projectile-browse-dirty-projects)
      ("s" "Switch Project" counsel-projectile-switch-project)
      ("o" "Switch Open Project" projectile-switch-open-project)
      ("X" "Remove Project" projectile-remove-current-project-from-known-projects)
      ("A" "Add project" projectile-add-known-project)
      ("Z" "Add projects in dir" projectile-discover-projects-in-directory)
      ]
     ["Buffers"
      ("f" "File or Buffer" counsel-projectile)
      ("b" "Switch open project buffer" counsel-projectile-switch-to-buffer)
      ("S" "Save project buffers" projectile-save-project-buffers)
      ("K" "Kill project buffers" projectile-kill-buffers)
      ("n" "Next project buffer" projectile-next-project-buffer)
      ("p" "Previous project buffer" projectile-previous-project-buffer)
      ]
     ["Action"
      ;; add a switch to use older projectile-ripgrep
      ("r" "Ripgrep" counsel-projectile-rg)
      ("v" "Vterm" counsel-projectile-vterm)
      ("d" "Dired" projectile-dired)

      ("p" "Replace" projectile-replace)
      ("P" "Replace regexp" projectile-replace-regexp)
      ]
     ["Manage"
      ;; TODO org agenda files does not pick up captured items
      ("a" "Org Agenda" counsel-projectile-org-agenda)
      ("c" "Org Capture" counsel-projectile-org-capture)
      ]
     ]
    )

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

  (transient-define-prefix pmx-window-controls ()
    "Window navigation and manipulation"
    ["Window Commands \n"
     ["Current"
      ("x" "Delete window" delete-window)
      ("X" "Delete frame" delete-frame)
      ("h" "Split horizontally" split-window-right)
      ("v" "Split vertically" split-window-below)
      ]
     ["Another"
      ("D" "Delete" switch-window-then-delete)
      ("M" "Maximize" switch-window-then-maximize)
      ("v" "Move" switch-window-then-swap-buffer)
      ]
     ["Rotate"
      ("w" "Windows" rotate-window)
      ("l" "Layout" rotate-layout)
     ]
     ["Jump"
      ("o" "Window" switch-window)
      ("f" "Frame" other-frame)
      ]
     ]
    )

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


(use-package general)

(use-package disable-mouse)

(defun ikaruga--disable-mouse-in-keymap (map)
  "Rebind all mouse commands in MAP so that they are disabled.
When INCLUDE-TARGETS is non-nil, also disable mouse actions that
target GUI elements such as the modeline."
  (dolist (binding (disable-mouse--all-bindings nil))
    (define-key map binding nil)))

(defun ikaruga--exclusive-keymap ()
  "Make a keymap with all keys defaulted to ignore.
Mouse keys are set to nil in order to pass through to maps of
lower precedence."
  ;; TODO allow user to customize the disable-mouse call.
  (let ((map (make-sparse-keymap)))
    (ikaruga--disable-mouse-in-keymap map)
    ;; TODO set to ignore.
    (define-key map [t] (lambda ()
                          (interactive)
                          (message "Event (hello): %s"
                                   (key-description (this-command-keys)))))
    map))

(defun ikaruga--unmodified-key (key)
  "Strip modifier from keys.
KEY is understood by kbd"
  (s-right 1 key))

(defun ikaruga--setup-independent-states (buffer)
  "Make ikaruga states buffer local for BUFFER.

This enables a buffer to undergo independent state switches."
  (make-local-variable 'ikaruga--entry-active)
  (make-local-variable 'ikaruga--command-active)
  (make-local-variable 'ikaruga-state))

(defun ikaurga--setup-independent-maps (buffer)
  "Make ikaruga keymaps buffer local for BUFFER.

After this operation, there will be buffer local values for
`emulation-mode-map-alists' `ikaruga-entry-map'
`ikaruga-command-map' `ikaruga--emulation-maps' and all of the
variables set by `ikaruga--setup-independent-states'.

This means that any keys added to the maps will only be mapped in
that buffer and any changes to the states or their bindings
within the `emulation-mode-map-alists' will only affect that
buffer.

This enables per-buffer ikaruga states and per-buffer ikaruga
behaviors to be implemented, provided that other configuration
per modes and in binding statements is also doing interesting
things."
  (ikaruga--setup-independent-states buffer)
  (make-local-variable emulation-mode-map-alists)
  (make-local-variable ikaruga-entry-map)
  (make-local-variable ikaruga-command-map)
  (make-local-variable ikaruga--emulation-maps)
  (setf emulation-mode-map-alists (copy-sequence (default-value 'emulation-mode-map-alists)))
  (delq emulation-mode-map-alists ikaruga--emulation-maps)
  (setf ikaruga-entry-map (make-sparse-keymap))
  (setf ikaruga-command-map (make-sparse-keymap))
  (set-keymap-parent ikaruga-entry-map (default-value 'ikaruga-entry-map))
  (set-keymap-parent ikaruga-command-map (default-value 'ikaruga-command-map))
  (setf ikaruga--emulation-maps
        `((ikaruga--entry-active . ,ikaruga-entry-map)
          (ikaruga--command-active . ,ikaruga-command-map)))
  (push ikaruga--emulation-maps emulation-mode-map-alists))

(defun ikaruga--setup-vterm ()
  "Set up a vterm buffer for ikaruga integration."
  (ikaruga--setup-independent-states (current-buffer))
  (ikaruga--state-insert (current-buffer)))

(defun ikaruga--enter-vterm ()
  "Update keymap states for vterm buffer entry."
  (message "Vterm was entered"))

(defun ikaruga--leave-vterm ()
  "Update keymap states after leaving vterm."
  (message "Vterm was left"))

(defgroup ikaruga nil
  "Ikaruga - bindings that polarize."
  :prefix 'ikaruga
  :group 'ikaruga)

;; (defcustom ikaruga-generic-major-mode-behavior
;;   '((:setup ikaruga--setup-default
;;      :enter ikaruga--enter-default
;;      :leave ikaruga--leave-default)))
;;   "Uses the same format as `ikaruga-custom-major-modes' values")

(defcustom ikaruga-custom-major-modes
  '((vterm-mode . (:derived t
                   :setup ikaruga--setup-vterm
                   :enter ikaruga--enter-vterm
                   :leave ikaruga--leave-vterm)))
    ;; (special-mode . (:derived t
    ;;                  :setup ikaruga--setup-special
    ;;                  :enter ikaruga--enter-special
    ;;                  :leave ikaruga--leave-special)))
  "Major modes with custom behavior.

List of alists.  Each element key is a major mode.  The values
are plists.  :setup will run whenever a buffer first becomes active.
:enter will run when that buffer becomes the `current-buffer'.
:leave will run when another buffer becomes current.

Whatever :enter does to `emulation-mode-map-alists' it must also
undo in :leave unless it creates buffer local versions of any
states that it modifies.

Whenever a mutable data structure is changed, it must also be
restored or later buffers will inherit the behavior of previous
buffers.  Use `make-local-variable' on anything before
modification to mitigate this issue."
  :group 'ikaruga
  :type '(alist :key-type symbol :value-type plist))

(defconst ikaruga-states
  '((const :tag "Insert" ikaruga-state-insert)
    (const :tag "Active" ikaruga-state-command)
    (const :tag "Special" ikaruga-state-special))
  "Ikaruga behavior states.

`ikaruga-state-insert' uses modified keys to execute commands.
Each command execution also changes the state to
`ikaruga-state-command'.  In `ikaruga-state-command', no
modifiers are necessary to use commands that were available by
modifier in `ikaruga-state-insert'.  Finally, the
`ikaruga-state-special' is for `special-mode' buffers, which
effectively have their own mode-specific command state by
default.")

(defcustom ikaruga-initial-state 'ikaruga-state-insert
  "State after turning on `ikaruga-mode' global minor mode."
  :group 'ikaruga
  :type `(choice ,ikaruga-states))

(defcustom ikaruga-minibuffer-setup-state  'ikaruga-state-insert
  "Temporary state turned on when a mininbuffer is setup."
  :group 'ikaruga
  :type `(choice ,ikaruga-states))

(defcustom ikaruga-reentry-in-command-state t
  "Entry keys will be bound in command state."
  :group 'ikaruga
  :type 'boolean)

(defvar ikaruga-state 'ikaruga-initial-state
  "Current state.")

(defvar ikaruga-polarity 'forward
  "`forward' or `backward' are valid valids.")

(defvar ikaruga-command-map (ikaruga--exclusive-keymap)
  "Ikaruga command state bindings.")

(defvar ikaruga--command-active nil
  "`ikaruga-command-map' is enabled if non-nil.")

(defvar ikaruga-entry-map (make-sparse-keymap)
  "Ikaruga insert state bindings, which enter command state.")

(defvar ikaruga--entry-active nil
  "`ikaruga-entry-map' is enabled if non-nil.")

(defvar ikaruga--emulation-maps
  `((ikaruga--entry-active . ,ikaruga-entry-map)
    (ikaruga--command-active . ,ikaruga-command-map))
  "Emulation maps.")

(defvar-local ikaruga--visited nil "Has this buffer run setup already?")

(defun ikaruga--minibuffer-setup ()
  "Set up any temporary minibuffer behavior & states."
  (if-let (minibuffer-window (active-minibuffer-window))
      (let ((minibuffer (window-buffer minibuffer-window))
            (minibuffer-state ikaruga-minibuffer-setup-state))
        (ikaruga--setup-independent-states minibuffer)
        ;; TODO pull this into function
        (cond ((eq minibuffer-state 'ikaruga-state-special)
               (ikaruga--state-special minibuffer))
              ((eq minibuffer-state 'ikaruga-state-insert)
               (ikaruga--state-insert minibuffer))
              ((eq minibuffer-state 'ikaruga-state-command)
               (ikaruga--state-command minibuffer))))
    (warn "minibuffer setup was called by `minibuffer-setup-hook' but no minibuffer was available")))

(defun ikaruga--minibuffer-exit ()
  "Clean up temporary minibuffer state."
  (message "minibuffer clean up"))

(defun ikaruga--window-switch ()
  "Hook called when maps need to be updated.

The `ikaruga-state' is the same, but some major modes and the
mininbuffer are handled different."
  (let* ((minibuffer (active-minibuffer-window))
         (active-buffer (current-buffer))
         (active-buffer-mode (buffer-local-value 'major-mode active-buffer))
         (visited (buffer-local-value 'ikaruga--visited active-buffer)))
    (message "Current buffer: %s" active-buffer)
    (if (minibufferp active-buffer)
        (message "In the mininbuffer.  No work necessary thanks to local-variables!")


      ;; TODO the buffer is not in whatever mode it will be in yet!
      ;; Check the mode hooks.
      (if-let ((custom-behavior (alist-get active-buffer-mode ikaruga-custom-major-modes)))
          (message "In a custom major mode %s" custom-behavior)
        (message "Normal behavior for mode: %s" active-buffer-mode)))))

(defun ikaruga--state-special (buffer)
  "This is basically a way to disable ikaruga in temporarily in BUFFER.
See `ikaruga-state-special'."
  (setf (buffer-local-value 'ikaruga--entry-active buffer) nil)
  (setf (buffer-local-value 'ikaruga--command-active buffer) nil)
  (setf (buffer-local-value 'ikaruga-state buffer) 'ikaruga-state-special))

(defun ikaruga-state-special ()
  "This is basically a way to disable ikaruga in temporarily."
  (interactive)
  (ikaruga--state-special (current-buffer)))

(defun ikaruga--state-insert (buffer)
  "Disable `ikaruga-command-map' in BUFFER.
See `ikaruga-state-insert'."
  (setf (buffer-local-value 'ikaruga--entry-active buffer) t)
  (setf (buffer-local-value 'ikaruga--command-active buffer) nil)
  (setf (buffer-local-value 'ikaruga-state buffer) 'ikaruga-state-insert))

(defun ikaruga-state-insert ()
  "Disable `ikaruga-command-map'.
Only modified keys, from `ikaruga-entry-map' will enter command state."
  (interactive)
  (ikaruga--state-insert (current-buffer)))

(defun ikaruga--state-command (buffer)
  "Enable unmodified keys from `ikaruga-command-map' in BUFFER.
See `ikaruga-state-command'."
  (setf (buffer-local-value 'ikaruga--entry-active buffer) ikaruga-reentry-in-command-state)
  (setf (buffer-local-value 'ikaruga--command-active buffer) t)
  (setf (buffer-local-value 'ikaruga-state buffer) 'ikaruga-state-command))

(defun ikaruga-state-command ()
  "Enable unmodified keys from `ikaruga-command-map'.

Call `ikaruga-state-insert' to exit the command state.  Set
`ikaruga-reentry-in-command' to nil if you want only
`ikaruga-command-map' active in command state."
  (interactive)
  (ikaruga-state-command (current-buffer)))

(defun ikaruga--add-hooks ()
  "Add hooks."
  (add-hook 'minibuffer-setup-hook #'ikaruga--minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'ikaruga--minibuffer-exit)
  (add-hook 'buffer-list-update-hook #'ikaruga--window-switch))

(defun ikaruga--remove-hooks ()
  "Remove hooks."
  (remove-hook 'minibuffer-setup-hook #'ikaruga--minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook #'ikaruga--minibuffer-exit)
  (remove-hook 'buffer-list-update-hook #'ikaruga--window-switch))

(defun ikaruga-define-key (key command)
  "Helper for adding new active & entry keys.

KEY is something understood by kbd.
COMMAND is a command."
  (define-key ikaruga-entry-map (kbd key) command)
  (define-key ikaruga-command-map (kbd (ikaruga--unmodified-key key)) command))

(define-minor-mode ikaruga-mode
  "Ikaruga polarized bindings"
  :global t
  :lighter ""
  :after-hook
  (if ikaruga-mode
      (progn
        (push ikaruga--emulation-maps emulation-mode-map-alists)
        (ikaruga--add-hooks)
        (if (equal ikaruga-initial-state 'ikaruga-state-insert)
            (ikaruga-state-insert)
          (ikaruga-state-command)))
    (prognii
      (ikaruga--remove-hooks)
      (delq ikaruga--emulation-maps emulation-mode-map-alists)
      (ikaruga--minibuffer-teardown))))

;; (delq ikaruga--emulation-maps emulation-mode-map-alists)
;; (setf ikaruga-command-map (ikaruga--exclusive-keymap))
;; (setf ikaruga--emulation-maps
;;   `((ikaruga--entry-active . ,ikaruga-entry-map)
;;     (ikaruga--command-active . ,ikaruga-command-map)))

;; (push ikaruga--emulation-maps emulation-mode-map-alists)
;; (define-key ikaruga-command-map (kbd "i") (lambda ()
;;                                            (interactive)
;;                                            (message "yes it's working")
;;                                            (ikaruga-state-insert)))
;; (define-key ikaruga-command-map (kbd "x") 'execute-extended-command)
;; (define-key ikaruga-entry-map (kbd "M-i") (lambda ()
;;                                             (interactive)
;;                                             (ikaruga-state-command)))
;; (ikaruga--add-hooks)
;; (ikaruga--remove-hooks)


;;; posimacs-bindings.el ends here

;;; posimacs-defaults --- Posimacs  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; These options are mostly independent little tweaks or packages that are not
;; themselves very interactive, such as ws-butler, or are basically essential
;; but not that dependent on other packages, such as envrc
;;

;;; Code:

;;; Some settings more pertinant to Elisp
;; Info walks all nodes
(setopt Info-scroll-prefer-subnodes t)
(setq-default comint-scroll-to-bottom-on-input t)
(show-paren-mode 1) ; Show matching parentheses
;;; End Elisp settings.  See posimacs-elisp.el

;;; Filling and truncation
;; Always truncate in prog modes, never in any other mode.
(setopt truncate-lines nil)
(setopt truncate-partial-width-windows nil) ; interacts with truncate-lines
(defun pmx--truncate-prog ()
  "Hook function."
  (setq-local truncate-lines t))
(add-hook 'prog-mode-hook #'pmx--truncate-prog)

;; Text modes use visual fill to 100 chars
;; Using hard newlines and other forms of filling interacts poorly with variable
;; pitch faces, and variable pitch faces are hat and possibly in use by others,
;; so I've adopted this setting to move towards org files that can be properly
;; displayed by clients.
(setopt visual-fill-column-width 80)

(setq-default fill-column 80) ; How wide to auto-fill paragraphs
;; Fill while typing by default in these modes
(add-hook 'fundamental-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
;;; End filling & truncation

(pixel-scroll-precision-mode 1)

;; Even if no buffers have unsaved changes, prompt before quitting
(setq confirm-kill-emacs 'y-or-n-p)

(with-eval-after-load 'server
  (unless (server-running-p)
    (server-start)))

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; See posimacs-bindings.el
(repeat-mode 1)

(setq select-enable-clipboard t) ; yanks copy to X clipboard
(setq scroll-step 1) ; Line by line scrolling
(put 'list-timers 'disabled nil) ; yes I would like to see timers
(setq use-dialog-box nil)

;; intermittent issues with multibyte characters?
;; https://www.reddit.com/r/emacs/comments/kgv4fj/problems_in_org_trello_error_requestcurlsync/
(setq request-backend 'url-retrieve)

;; Most links you see in Emacs don't need full browsers
(setq browse-url-browser-function 'eww-browse-url)

;; TODO need to check on faster workflows to display formatted text for development.
(setq message-strip-special-text-properties nil)
(setq minibuffer-message-properties nil)

;; don't ask to spell out "yes"
(setopt use-short-answers t)

;; don't show transient commands in M-x completions?
;; (setq read-extended-command-predicate
;;       'command-completion-default-include-p)

;; add to transient package
(setopt transient-hide-during-minibuffer-read t)

;; sweet
(setq frame-resize-pixelwise t)

(setq-default indent-tabs-mode nil) ; tabs are not real

;; Use recycle bin or whatever
(setq delete-by-moving-to-trash t)

;; Lock files annoying.  The extensive use of Emacs server makes it highly
;; unlikely to wind up with two Emacs looking at the same files.  Take care with
;; multiple Emacsen to point them to another directory.
(setq create-lockfiles nil)

;; deprecate the really crappy list-buffers mode
;; ibuffer is more comprehensive than the filtered counsel switch buffer
(substitute-key-definition 'list-buffers 'ibuffer global-map)

(delete-selection-mode 1) ; Actions on active region will delete

(column-number-mode 1) ; show columns in modeline

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)

(setq scroll-error-top-bottom t)

;; nicer regex
(setq reb-re-syntax 'read)
(setq show-trailing-whitespace t)

;; Why did I ever allow this to be set to nil?
(setq enable-recursive-minibuffers t)

(blink-cursor-mode -1)
;; (setq blink-cursor-interval 0.14
;;       blink-cursor-delay 0.5
;;       blink-cursor-blinks 300)

;; Thanks Steve Purcell
(keymap-global-set "RET" 'newline-and-indent)

(defun pmx-keyboard-quit ()
  "Quit or close minibuffer if open."
  (interactive)
  ;; (declare-function minibuffer-keyboard-quit "delsel" ())
  (if (active-minibuffer-window)
      ;; This way is generic across multiple frames but our setup isn't
      ;; typically using multiple frames yet.
      (progn (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
             (select-window (active-minibuffer-window))
             (minibuffer-keyboard-quit))
    (keyboard-quit)))

;; Navigate to minibuffer before executing quit if necessary
(keymap-global-set "C-g" 'pmx-keyboard-quit)

;; Using M-g everywhere instead of C-g
(keymap-set key-translation-map "M-g" "C-g")

;; visit files at same position
(save-place-mode 1)

;; Turn off initial scratch
(setq initial-scratch-message nil)

;;;;;;;;;;;;;;;;;;;;
;; Basic packages ;;
;;;;;;;;;;;;;;;;;;;;

;; Accumulate history of buffers automatically.  Saves mental effort enormously.
(use-package recentf
  :demand t
  :after no-littering
  :elpaca nil
  :config
  (recentf-load-list)
  (setq recentf-max-saved-items 400)

  ;; bookmarks are a workaround for fine-grained history for files in these
  ;; directories
  ;; (add-to-list 'recentf-exclude
  ;;              (recentf-expand-file-name no-littering-var-directory))
  ;; (add-to-list 'recentf-exclude
  ;;              (recentf-expand-file-name no-littering-etc-directory))

  ;; Nix paths will frequently be opened when viewing documentation or source.
  (add-to-list 'recentf-exclude "/nix/store/")

  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package ws-butler                  ; Cleanup whitespace at end of lines
  :config
  ;; TODO upstream a fix to make this more aggressive after yanks
  (ws-butler-global-mode t))

;; camel case counts as words boundaries when navigating.  symbol navigation
;; still leaps by whole words.
;; (use-package subword
;;   :delight
;;   :elpaca nil                           ; built-in package
;;   :config
;;   (global-subword-mode 1))

;; TODO move this to a package of enhancements to baseline behavior
(use-package command-log
  :elpaca (command-log
           :host github
           :repo "positron-solutions/command-log")
  :custom
  (command-log-window-text-scale 2 "Command log two steps higher text scale")
  (command-log-logging-shows-buffer t "Toggling will show the buffer.")
  (command-log-hiding-disables-logging t "Toggling visible buffer turns off logging.")
  (command-log-disabling-logging-kills-buffer t "The buffer will be new when
displayed again.")
  (command-log-log-globally
   t
   "Auto-enable with global minor mode (including minibuffer)")

  :config
  (setopt command-log-default-side 'left)
  (setopt command-log-filter-commands '(self-insert-command))
  (setopt command-log-merge-repeat-targets 'post-command))

;; TODO find other packages that are not being managed by elpaca and bring them under the law
;; https://www.reddit.com/r/emacs/comments/okse5o/magit_not_accepting_cnp_or_updown_arrows/
;; See `list-load-path-shadows'.
(use-package project :elpaca nil)

;;; posimacs-defaults.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

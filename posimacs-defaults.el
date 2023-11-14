;;; posimacs-defaults --- Posimacs  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; These options are mostly independent little tweaks or packages that are not
;; themselves very interactive, such as ws-butler, or are basically essential
;; but not that dependent on other packages, such as direnv
;;

;;; Code:
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t))) ;; only truncate prog-mode, wrap-text mode
(setq truncate-partial-width-windows nil) ; interacts with truncate-lines

;; Even if no buffers have unsaved changes, prompt before quitting
(setq confirm-kill-emacs 'y-or-n-p)

;; Unbind Pesky Sleep Button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

(setq select-enable-clipboard t) ; yanks copy to X clipboard
(setq scroll-step 1) ; Line by line scrolling

(setq-default fill-column 80) ; How wide to auto-fill paragraphs

(put 'list-timers 'disabled nil) ; yes I would like to see timers

(setq use-dialog-box nil)

;; intermittent issues with multibyte characters?
;; https://www.reddit.com/r/emacs/comments/kgv4fj/problems_in_org_trello_error_requestcurlsync/
(setq request-backend 'url-retrieve)

;; Most links you see in Emacs don't need full browsers
(setq browse-url-browser-function 'eww-browse-url)

(setq message-strip-special-text-properties nil)
(setq minibuffer-message-properties nil)

;; don't ask to spell out "yes"
(setopt use-short-answers t)

(setopt native-comp-async-report-warnings-errors 'silent)

;; don't show transient commands in M-x completions?
;; (setq read-extended-command-predicate
;;       'command-completion-default-include-p)

;; add to transient package
(setq transient-hide-during-minibuffer-read t)

;; Accumulate history of buffers automatically.  Saves mental effort enormously.
(use-package recentf
  :demand t
  :after no-littering
  :elpaca nil
  :config
  (recentf-load-list)
  (setq recentf-max-saved-items 200)

  ;; bookmarks are a workaround for fine-grained history for files in these
  ;; directories
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name no-littering-etc-directory))

  (run-at-time nil (* 5 60) 'recentf-save-list))

(setq-default indent-tabs-mode nil) ; tabs are not real

;; Use recycle bin or whatever
(setq delete-by-moving-to-trash t)

;; Move to bottom
(setq-default comint-scroll-to-bottom-on-input t)

;; LISP
(setq load-prefer-newer t)

;; Lock files annoying
(setq create-lockfiles nil)

;; deprecate the really crappy list-buffers mode
;; ibuffer is more comprehensive than the filtered counsel switch buffer
(substitute-key-definition 'list-buffers 'ibuffer global-map)

(show-paren-mode 1) ; Show matching parentheses

(delete-selection-mode 1) ; Actions on active region will delete

(column-number-mode 1) ; show columns in modeline

;;;;;;;;;;;;;;;;;;;;
;; Basic packages ;;
;;;;;;;;;;;;;;;;;;;;

;; TODO use envrc
(use-package direnv ; direnv integration
  :after lsp
  :delight 'direnv-mode
  :config
  ;; Ensures that external dependencies are available before they are called.
  (add-hook 'prog-mode-hook #'direnv--maybe-update-environment)
  ;; XXX cargo culted.  Investigate
  (add-to-list 'direnv-non-file-modes 'vterm-mode)
  (setq direnv-always-show-summary nil)
  (direnv-mode 1))

(use-package ws-butler ; Cleanup whitespace at end of lines
  :delight
  :config (ws-butler-global-mode t))

;; camel case counts as words boundaries when navigating.  symbol navigation
;; still leaps by whole words.
(use-package subword
  :delight
  :elpaca nil                           ; built-in package
  :config
  (global-subword-mode 1))

;; TODO move this to a package of enhancements to baseline behavior
(use-package command-log
  :elpaca (command-log
           :host github
           :repo "positron-solutions/command-log")
  :custom
  (command-log-window-text-scale 2 "Command log two steps higher text scale")
  (command-log-logging-shows-buffer t "Toggling will show the buffer.")
  (command-log-hiding-disables-logging t "Toggling visible buffer turns off logging.")
  (command-log-disabling-logging-kills-buffer t "The buffer will be new when displayed again.")
  (command-log-log-globally t "Auto-enable with global minor mode (including minibuffer)")
  (command-log-filter-commands '(self-insert-command) "Be chatty. Show everything besides self-insert-command"))

;; TODO find other packages that are not being managed by elpaca and bring them under the law
;; https://www.reddit.com/r/emacs/comments/okse5o/magit_not_accepting_cnp_or_updown_arrows/
(use-package project :elpaca nil)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq scroll-error-top-bottom t)

;; nicer regex
(setq reb-re-syntax 'read)
;; visually replace with overlays
(use-package visual-regexp)

;; (setq browse-url-browser-function (lambda (url) (eww-browse-url url t)))

(put 'erase-buffer 'disabled nil)

(setq show-trailing-whitespace t)

;; Why did I ever allow this to be set to nil?
(setq enable-recursive-minibuffers t)

(setq blink-cursor-interval 0.2
      blink-cursor-delay 0.2
      blink-cursor-blinks 150)

;; Thanks Steve Purcell
(global-set-key (kbd "RET") 'newline-and-indent)


;;; posimacs-defaults.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

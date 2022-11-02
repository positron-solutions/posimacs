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

;; focus help window so that it can be closed immediately with 'q'
(setq help-window-select t)

(setq-default fill-column 100) ; How wide to auto-fill paragraphs

(setq-default indent-tabs-mode nil) ; tabs are not real

;; Use recycle bin or whatever
(setq delete-by-moving-to-trash t)

(setq flyspell-mode t)

;; LISP
(setq load-prefer-newer t)

;; Lock files annoying
(setq create-lockfiles nil)

;; deprecate the really crappy list-buffers mode
;; ibuffer is more comprehensive than the filtered counsel switch buffer
(substitute-key-definition 'list-buffers 'ibuffer global-map)

(show-paren-mode 1) ; Show matching parentheses

(delete-selection-mode 1) ; Actions on active region will delete


;;;;;;;;;;;;;;;;;;;;
;; Basic packages ;;
;;;;;;;;;;;;;;;;;;;;

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

(use-package subword ; camel case counts as words boundaries when navigating
  :delight
  :config
  (global-subword-mode 1))

;; has a binding in help
(use-package command-log-mode
  :custom
  (clm-window-text-scale 2 "Command log two steps higher text scale")
  (clm-logging-shows-buffer t "Toggling will show the buffer.")
  (clm-hiding-disables-logging t "Toggling visible buffer turns off logging.")
  (clm-disabling-logging-kills-buffer t "The buffer will be new when displayed again.")
  (clm-log-globally t "Auto-enable with global minor mode (including minibuffer)")
  (clm-exceptions '(self-insert-command) "Be chatty.
        Show everything besides self-insert-command"))

;; TODO find other packages that are not being managed by straight and bring them under the law
;; https://www.reddit.com/r/emacs/comments/okse5o/magit_not_accepting_cnp_or_updown_arrows/
(use-package project)

;;;;;;;;;;;;;;;;;;;;;;;
;; Window Management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Auto-balancing and less aggressive automatic window splitting are a
;; pre-requisite for any sane window management strategy.

;; Windows are only eligible to be split horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 120)

(defun balance-windows--advice (&rest _ignored)
  "Balance windows (intended as :after advice); any args are ignored."
  (balance-windows))

;; Balance after splits and removals
(advice-add 'split-window-right :after #'balance-windows--advice)
(advice-add 'split-window-below :after #'balance-windows--advice)
(advice-add 'delete-window :after #'balance-windows--advice)

(defun pmx-split-window-conservatively (&optional window)
  "Split WINDOW only if absolutely necessary.
Only split if there is no split, and only split into left & right
windows.  If no window is specified then WINDOW defaults to
output of 'selected-window'.  'split-width-threshold' is
observed."
  (interactive)
  (let ((window (or window (selected-window))))
    (if (and
         (window-splittable-p window t)
         (eq (length (window-list)) 1))
        (with-selected-window window
          (split-window-right))
      nil)))

(setq split-window-preferred-function #'pmx-split-window-conservatively)

;;;;;;;;;;;;;;;;;;;;;
;; Window Controls ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package windmove
  :config
  (setq windmove-wrap-around t))

(use-package switch-window
  :config
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-minibuffer-shortcut ?m)
  (setq switch-window-input-style 'read-event)
  (setq switch-window-background nil)
  (setq switch-window-qwerty-shortcuts '("p" "o" "s" "i" "t" "r" "a" "c" "x" "y" "v" "u")))

(use-package rotate) ; rotate windows and layouts

(use-package avy ; fast-cursor-jumping in buffer visible area
  :custom
  (setq avy-escape-chars '(?\e ?\C-g))
  :config
  (setq avy-all-windows 'all-frames)) ; avy can switch frames

;;;;;;;;;;;;;;
;; Controls ;;
;;;;;;;;;;;;;;

(use-package default-text-scale
  :delight default-text-scale-mode
  :config
  (default-text-scale-mode)) ;C-M-= and C-M-- for larger and smaller text

;;;;;;;;;;;;;;
;; Modeline ;;
;;;;;;;;;;;;;;

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (add-hook 'after-make-frame-functions #'graphical-frame-icons)
  :custom
  (doom-modeline-buffer-encoding nil "Nobody reads encoding.")
  (doom-modeline-major-mode-icon t "This is probably default.")
  (doom-modeline-env-version nil "Rust 1.45.whocares."))

(defun graphical-frame-icons (frame)
  "Activate doom-modeline icons if FRAME has graphics."
  (select-frame frame)
  (setq doom-modeline-icon (display-graphic-p frame)))

;; A slightly more informative scratch buffer
(setq initial-scratch-message nil)

;; https://github.com/Malabarba/beacon
;; When you scroll, the cursor highlights very loudly
(use-package beacon
  :delight beacon-mode
  :config
  (beacon-mode))

;; When do we ever want to confirm after requesting execution?
(setq org-confirm-babel-evaluate nil)

;; Fill while typing by default in text modes
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'fundamental-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'markdown-mode-hook #'auto-fill-mode)



(defvar pmx-help-modes '(helpful-mode
                         help-mode
                         Man-mode
                         woman-mode
                         Info-mode))

(defun pmx-buffer-help-p (buf act)
  "BUF is a help buffer, ignore ACT."
  (member (buffer-local-value 'major-mode (get-buffer buf)) pmx-help-modes))

(add-to-list 'display-buffer-alist
             `(pmx-buffer-help-p         ;predicate
               (display-buffer--maybe-same-window
                display-buffer-reuse-window
                display-buffer-reuse-mode-window) ;functions to try
               (mode . ,pmx-help-modes)
               (inhibit-same-window . nil)))

;;; posimacs-defaults ends here

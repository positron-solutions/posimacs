;;; posimacs-windows --- Posimacs  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Window management out of the box can be hideous.  Automating window
;; management is something almost every user needs to do according to a scheme
;; that suits their purpose.
;;
;; The scheme implemented in Posimacs favors a 2k or 4k display single screen
;; situation.  Starting from one window, the configuration will scale up to two
;; left & right windows automatically and then begin re-using them such that
;; buffers rarely appear in different windows and buffers with similar modes
;; have a strong affinity.  If an explicit command changes a state, it should be
;; preserved in future mutations.  This creates a flexible and robust DWIM
;; system with predictable results.

;;; Code:

;; TODO Nyxt or something
;; (setq browse-url-browser-function (lambda (url) (eww-browse-url url t)))

(setq ediff-split-window-function 'split-window-horizontally)

(setopt scroll-margin 12)
(setopt maximum-scroll-margin 0.2)
(setopt scroll-down-aggressively 0.3)
(setopt scroll-up-aggressively 0.3)
(setopt scroll-conservatively 12)
(setopt pixel-scroll-mode t)

(setopt pixel-scroll-precision-interpolate-mice t)
(setopt pixel-scroll-precision-use-momentum t)
(setopt pixel-scroll-precision-interpolate-page t)

;; TODO Mouse rubber banding
;; TODO Mouse does not move point

;; This section implements anti-herky-jerk.  Herky jerk is when the opening the
;; minibuffer or a transient window causes windows to automatically scroll.  In
;; extremely suboptimal configurations, every buffer on the screen will
;; herky-jerk over repeated M-x invocations, causing epilepsy in small children.
(setq scroll-preserve-screen-position nil)
(setq scroll-error-top-bottom nil)
(setq next-screen-context-lines 12)
;; Dynamically set `scroll-conservatively' to avoid automatic re-centers.  We
;; want automatic re-center when swiper for example goes off screen, but for
;; minibuffer or transient display, we want the minimum possible scroll.  And
;; finally, dynamically shrink the margin so that the space created by margin
;; will all be used up before automatic scrolling can occur and margins will not
;; be maintained during minibuffer or transient display.
(defvar pmx--no-herky-jerk-margin nil)
(defvar pmx--no-herky-jerk-scroll-conservatively nil)
(defun pmx--no-herky-jerk-enter (&rest _)
  "Shrinks margin."
  (unless pmx--no-herky-jerk-scroll-conservatively
    (setq pmx--no-herky-jerk-scroll-conservatively scroll-conservatively)
    (setq scroll-conservatively 101))
  (unless pmx--no-herky-jerk-margin
    (setq pmx--no-herky-jerk-margin scroll-margin)
    (setq scroll-margin 0)))

(defun pmx--no-herky-jerk-exit ()
  "Expands margin."
  (when pmx--no-herky-jerk-scroll-conservatively
    (setq scroll-conservatively pmx--no-herky-jerk-scroll-conservatively))
  (setq pmx--no-herky-jerk-scroll-conservatively nil)
  (when pmx--no-herky-jerk-margin
    (setq scroll-margin pmx--no-herky-jerk-margin))
  (setq pmx--no-herky-jerk-margin nil))

(add-hook 'minibuffer-setup-hook #'pmx--no-herky-jerk-enter)
(add-hook 'minibuffer-exit-hook #'pmx--no-herky-jerk-exit)

;; Add the same for transient
(with-eval-after-load 'transient
  (advice-add 'transient-setup :before #'pmx--no-herky-jerk-enter)
  (add-hook 'transient-exit-hook #'pmx--no-herky-jerk-exit)
  (setopt transient-hide-during-minibuffer-read t))

(setq warning-display-at-bottom nil)

;; Auto-balancing and less aggressive automatic window splitting are a
;; prerequisite for any sane window management strategy.

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
         (= (length (window-list)) 1))
        (with-selected-window window
          (split-window-right))
      nil)))

(setq split-window-preferred-function #'pmx-split-window-conservatively)

(use-package windmove
  :ensure nil
  :config

  ;; Stack overflow was decent!
  ;; https://stackoverflow.com/questions/25249669/emacs-windmove-move-a-buffer-without-switching
  ;; TODO make a more DWIM solution
  (defun pmx-slide-buffer (dir)
    "Move current buffer into window at direction DIR.
DIR is handled as by `windmove-other-window-loc'."
    (interactive
     (list (intern (completing-read "Direction: " '(left right up down)))))
    (let ((buffer (current-buffer))
          (target (windmove-find-other-window dir)))
      (if (null target)
          (user-error "There is no window %s from here" dir)
        (switch-to-prev-buffer)
        (select-window target)
        (switch-to-buffer buffer nil t))))

  (setq windmove-wrap-around t))

;; Rotate window layouts.  Occasionally helpful for fixing up a mishap.
(use-package rotate)

;; The ultimate DWIM other window command.
(defvar pmx--direction -1)
(defvar pmx--last-win nil)
(defun pmx-other-window (frame)
  "Switch window, with DWIM behavior.
Prefix argument FRAME will unconditionally switch frames.
When called without any windows to switch to, split and select.
If called not in repeat, reverse directions and switch back to
usually the most recent window (though not `get-mru-window').
Finally, when called in repeat, continue in the same direction so
that we can usually get to the right window faster than an `avy'
call unless there's a ton of windows for some reason."
  (interactive "P")
  (cond (frame (other-frame 1))          ; unconditional with prefix arg
        ((equal 1 (length (window-list
                           (selected-frame))))
         ;; If there is no window or even minibuffer open, split window.  Change
         ;; the direction so that we go back to the source window on repeat or
         ;; next call.
         (let ((source (selected-window))
               (tall (> (frame-pixel-height) (frame-pixel-width))))
           (select-window (if tall
                              (split-window-below)
                            (split-window-right)))
           (if (eq source (next-window))
               (setq pmx--direction 1)
             (setq pmx--direction -1)
             (when (not (eq source (previous-window)))
               (warn "Split window sucessor inconsistent")))))
        ((not (eq last-command 'pmx-other-window))
         ;; If we are not repeating an other-window command, reverse the
         ;; direction and select in that direction.
         (if (eq pmx--last-win (selected-window))
             (setq pmx--direction (- pmx--direction))
           ;;  we changed windows out of band. Reverse directions.
           (setq pmx--direction -1))
         (other-window pmx--direction))
        (t
         ;; We are repeating.  Continue going in the established direction.
         (other-window pmx--direction)))
  (setq pmx--last-win (selected-window)))

(keymap-global-set "M-o" #'pmx-other-window)

;; For all arbitrary text navigation and window changes, avy is used
(use-package avy
  :custom
  (setq avy-escape-chars '(?\e ?\C-g))
  :config
  (setq avy-all-windows 'all-frames)) ; avy can switch frames

;; focus help window so that it can be closed immediately with 'q'
(setq help-window-select t)

;; Make help buffers attempt to re-use a window when popping
(defvar pmx-other-win-modes
  '(helpful-mode help-mode shortdoc-mode Man-mode woman-mode
                 Info-mode elpaca-ui-log elisp-compile apropos-mode))

(defun pmx-buffer-help-p (buf act)
  "BUF is a help buffer, ignore ACT."
  (member (buffer-local-value 'major-mode (get-buffer buf)) pmx-other-win-modes))

(add-to-list 'display-buffer-alist
             `(pmx-buffer-help-p       ;predicate
               (display-buffer--maybe-same-window
                display-buffer-reuse-window
                display-buffer-reuse-mode-window) ;functions to try
               (mode . ,@pmx-other-win-modes)
               (inhibit-same-window . nil)))

;; Removed options that create frames
(setq display-buffer-fallback-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer-in-previous-window
         display-buffer-use-some-window)))

;;; posimacs-windows.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

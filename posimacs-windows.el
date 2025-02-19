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

(setopt scroll-margin 0)
(setopt scroll-down-aggressively 0.3)
(setopt scroll-up-aggressively 0.3)
(setopt scroll-conservatively 12)
(pixel-scroll-precision-mode 1)

(setopt pixel-scroll-precision-interpolate-mice t)
(setopt pixel-scroll-precision-use-momentum t)
(setopt pixel-scroll-precision-interpolate-page t)

;; This section implements anti-herky-jerk.  Herky jerk is when the opening the
;; minibuffer or a transient window causes windows to automatically scroll.  In
;; extremely suboptimal configurations, every buffer on the screen will
;; herky-jerk over repeated M-x invocations, causing epilepsy in small children.
(setq scroll-preserve-screen-position nil)
(setq scroll-error-top-bottom nil)
(setopt warning-display-at-bottom nil)
(setq next-screen-context-lines 12)

;; Eliminate stupid window movements caused by minibuffer or transient opening
;; and closing.
(defcustom pmx-no-herky-jerk-margin 12
  "Number of lines to protect from incidental scrolling.
A good value is the maximum height of your minibuffer, such as
configured by `ivy-height' and similar variables that configure packages
like `vertico' and `helm'."
  :type 'integer
  :group 'scrolling)

;; You would think we need multiple restore points.  However, there seems to be
;; a behavior where window points in non-selected windows are restored all the
;; time.  This was only apparent after moving them.
(defvar pmx--no-herky-jerk-restore nil
  "Where to restore selected buffer point.
List of BUFFER WINDOW SAFE-MARKER and RESTORE-MARKER.")

;; Counting line height would be more correct.  In general, lines are taller but
;; not shorter than the default, so this is a conservative approximation that
;; treats all lines as the default height.
(defun pmx--no-herky-jerk-enter (&rest _)
  "Adjust window points to prevent implicit scrolling."
  (unless (> (minibuffer-depth) 1)
    (let ((windows (window-at-side-list
                    (window-frame (selected-window))
                    'bottom))
          ;; height of default lines
          (frame-char-height (frame-char-height
                              (window-frame (selected-window)))))
      (while-let ((w (pop windows)))
        (with-current-buffer (window-buffer w)
          (let* ((current-line (line-number-at-pos (window-point w)))
                 (end-line (line-number-at-pos (window-end w)))
                 (window-pixel-height (window-pixel-height w))
                 (window-used-height (cdr (window-text-pixel-size
                                           w (window-start w) (window-end w))))
                 (margin-height (* frame-char-height pmx-no-herky-jerk-margin))
                 (unsafe-height (- window-used-height
                                   (- window-pixel-height margin-height)))
                 (unsafe-lines (+ 2 (ceiling (/ unsafe-height frame-char-height))))
                 (exceeded-lines (- unsafe-lines (- end-line current-line))))
            (when (> exceeded-lines 0)
              ;;  save value for restore
              (let* ((buffer (window-buffer w))
                     (restore-marker (let ((marker (make-marker)))
                                       ;; XXX this may error?
                                       (set-marker marker (window-point w)
                                                   buffer)))
                     (safe-point (progn
                                   (goto-char restore-marker)
                                   ;; XXX goes up too many lines when skipping
                                   ;; wrapped lines
                                   (ignore-error '(beginning-of-buffer
                                                   end-of-buffer)
                                     (previous-line exceeded-lines t))
                                   (end-of-line)
                                   (point))))
                (set-window-point w safe-point)
                (when (eq w (minibuffer-selected-window))
                  (let ((safe-marker (make-marker)))
                    (set-marker safe-marker safe-point buffer)
                    (setq pmx--no-herky-jerk-restore
                          (list buffer w safe-marker restore-marker))))
                (goto-char (marker-position restore-marker))))))))))

(defun pmx--no-herky-jerk-exit ()
  "Restore window points that were rescued from implicit scrolling."
  (when (and pmx--no-herky-jerk-restore
             (= (minibuffer-depth) 1)
             (null (transient-active-prefix)))
    (when-let* ((restore pmx--no-herky-jerk-restore)
                (buffer (pop restore))
                (w (pop restore))
                (safe-marker (pop restore))
                (restore-marker (pop restore)))
      (when (and (window-live-p w)
                 (eq (window-buffer w) buffer)
                 (= (window-point w) (marker-position safe-marker)))
        (goto-char restore-marker)
        (set-window-point w restore-marker))
      (set-marker restore-marker nil)
      (set-marker safe-marker nil)
      (setq pmx--no-herky-jerk-restore nil))))

;;;###autoload
(define-minor-mode pmx-no-herky-jerk-mode
  "Guard the point from unintended and stupid scrolling"
  :group 'pmx-no-herky-jerk
  :global t
  (cond
   (pmx-no-herky-jerk-mode
    (add-hook 'minibuffer-setup-hook #'pmx--no-herky-jerk-enter)
    (add-hook 'minibuffer-exit-hook #'pmx--no-herky-jerk-exit))
   (t
    (remove-hook 'minibuffer-setup-hook #'pmx--no-herky-jerk-enter)
    (remove-hook 'minibuffer-exit-hook #'pmx--no-herky-jerk-exit))))

;; Add the same for transient
(with-eval-after-load 'transient
  (advice-add 'transient-setup :before #'pmx--no-herky-jerk-enter)
  (add-hook 'transient-exit-hook #'pmx--no-herky-jerk-exit)
  (setopt transient-hide-during-minibuffer-read t))

;; Auto-balancing and less aggressive automatic window splitting are a
;; prerequisite for any sane window management strategy.

;; Windows are only eligible to be split horizontally
(setq split-height-threshold nil)
(setq split-width-threshold 150)

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
           (select-window (split-window-right))
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

;; TODO if scroll impossible, move point
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
         display-buffer-pop-up-window
         display-buffer-use-some-window)))

;;; posimacs-windows.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

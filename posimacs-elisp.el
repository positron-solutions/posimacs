;;; posimacs-elisp --- Making elisp hacking better out of the box -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The out-of-box experience with elisp is extremely conservative.  Editing,
;; introspecting, and maintaining projects should all be things Emacs is very
;; competent at.  While experienced users may opt for different or less things,
;; it's unreasonable to assume unfamiliar Emacs users want a bare bones
;; experience.
;;
;; Some pure-style configurations can be found in posimacs-style.el

;;; Code:

(use-package ielm
  :elpaca nil
  :config
  ;; When the output is huge, ielm can slow down.  Instead, let's just get rid
  ;; of large output since there are other ways to handle this better and
  ;; usually the user will change approach when the output is impractical.
  (add-hook #'inferior-emacs-lisp-mode
            (lambda () (setq-local comint-max-line-length 1024)
              ;; comint-output-filter-functions is permanent local
              (unless (memq #'comint-truncate-buffer comint-output-filter-functions)
                (push #'comint-truncate-buffer comint-output-filter-functions)))))

(use-package lispy
  :hook ((lisp-mode emacs-lisp-mode)))

;; Seeing delimiter balance at all times is pretty useful.
(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode)))

(use-package macrostep) ; macro inspection

;; This package makes all help buffers much more informative and easier to read.
;; This makes it much easier to introspect emacs state and elisp code.
(use-package helpful
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key))

  :demand t
  :config
  ;; patch apropos buttons to call helpful instead of help
  (with-eval-after-load 'apropos
    (dolist (fun-bt '(apropos-function apropos-macro apropos-command))
      (button-type-put
       fun-bt 'action
       (lambda (button)
         (helpful-callable (button-get button 'apropos-symbol)))))
    (dolist (var-bt '(apropos-variable apropos-user-option))
      (button-type-put
       var-bt 'action
       (lambda (button)
         (helpful-variable (button-get button 'apropos-symbol))))))

  ;; override regular help menu
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  (with-eval-after-load 'counsel
    (setq counsel-describe-function-function #'helpful-callable)
    (setq counsel-describe-variable-function #'helpful-variable)))

(defun pmx-buffer-ert-p (buf act)
  "BUF is an ert results buffer, ignore ACT."
  (eq (buffer-local-value 'major-mode (get-buffer buf)) 'ert-results-mode))

(use-package ert
  :elpaca nil
  :config
  (add-to-list 'display-buffer-alist
               `(pmx-buffer-ert-p         ;predicate
                 (display-buffer-reuse-mode-window) ;functions to try
                 (inhibit-same-window . nil))))

(use-package auto-compile)
;; pmx-ielm-this-buffer is a shortcut to open an ielm buffer for introspective
;; hacking on that buffer.  This is often faster than the scratch buffer
;; workflow, which both accumulates broken forms and requires manually
;; specifying which buffer you want to operate on.
(defun pmx--mru-buffer ()
  "Get the buffer of the most recently used window."
  (window-buffer (get-mru-window t t t)))

(defun pmx--not-ielm-buffer-p (buffer)
  "Filter predicate to remove ielm buffers from `read-buffer' candidates.

BUFFER candidate"
  (not (eq (buffer-local-value 'major-mode buffer)
           'inferior-emacs-lisp-mode)))

(defun pmx-ielm-this-buffer (&optional target-buffer)
  "Hack this buffer's locals with elisp in ielm.

TARGET-BUFFER buffer whose locals we want to hack

If the TARGET-BUFFER is already an ielm buffer, warn instead.
When TARGET-BUFFER is nil, use the result of `current-buffer'"
  (interactive)
  (let* ((target-buffer (or target-buffer (current-buffer)))
         (new-buffer-name (format "*ielm-<%s>*" target-buffer)))
    (if (pmx--not-ielm-buffer-p target-buffer)
      (progn (pop-to-buffer (get-buffer-create new-buffer-name))
             (ielm new-buffer-name)
             (ielm-change-working-buffer target-buffer))
      (user-error "Target buffer %s is already an ielm buffer" (buffer-name target-buffer)))))

(global-set-key (kbd "M-i") 'pmx-ielm-this-buffer)

;; Surprised this isn't in core already
;; https://emacs.stackexchange.com/questions/24657/unadvise-a-function-remove-all-advice-from-it
(defun pmx-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(defun pmx-dump-object-string (obj)
  "Return a string contents of slots of OBJ."
  (mapconcat
   (lambda (i) (format "%s: %s" i
                  (if (slot-boundp obj i)
                      (eieio-oref obj i)
                    eieio--unbound)))
   (object-slots obj) "\n"))

;; TODO Requires dash
(defun pmx-dump-object (obj)
  "Return a plist of slot-value pairs of eieio OBJ."
   (->>
    (object-slots obj)
    (--map (list it (if (slot-boundp obj it)
                        (eieio-oref obj it)
                      eieio--unbound)))
    (-concat)))

(provide 'posimacs-elisp)
;;; posimacs-elisp.el ends here

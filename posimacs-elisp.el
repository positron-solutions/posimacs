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
;; Completions and more general prog-mode configuration is in posimacs-prog.el

;;; Code:
(use-package elisp-mode
  :elpaca nil
  :config
  ;; On save, check parens
  (defun pmx--setup-check-paren-on-save ()
    (add-hook 'after-save-hook #'check-parens nil t))
  (add-hook 'emacs-lisp-mode-hook #'pmx--setup-check-paren-on-save))

(with-eval-after-load 'company
  (defun pmx--company-elisp-tweaks ()
    (setq-local company-backends '((company-capf
                                    company-dabbrev-code)
                                   (company-dabbrev-code company-gtags company-etags company-keywords)
                                   company-dabbrev)))
  (with-eval-after-load 'elisp-mode
    (add-hook 'emacs-lisp-mode-hook #'pmx--company-elisp-tweaks)
    (add-hook 'lisp-interaction-mode-hook #'pmx--company-elisp-tweaks))
  (with-eval-after-load 'ielm
    (add-hook 'ielm-mode-hook #'pmx--company-elisp-tweaks)))

(use-package package-lint)



(use-package elisp-depend)

(use-package flycheck-package
  :config (flycheck-package-setup))

(use-package ielm
  :elpaca nil
  :config
  (defvar pmx--ielm-ring nil)           ; one global ring
  (defun pmx-ielm-init-history ()
    "Persist ielm.
This is per ielm name, so multiple ielm's will get their own
history."
    (let* ((file (format "ielm/%s"  (buffer-name)))
           (path (no-littering-expand-var-file-name file)))
      (unless (file-exists-p path)
        (make-empty-file path 'parents))
      (setq-local comint-input-ring-file-name path)
      (setq-local comint-input-ring-size 1024)
      (setq-local comint-input-ignoredups t)
      (if pmx--ielm-ring
          (setq-local comint-input-ring pmx--ielm-ring)
        (comint-read-input-ring)
        (setq pmx--ielm-ring comint-input-ring))))
  (add-hook 'ielm-mode-hook 'pmx-ielm-init-history)

  (defun pmx--ielm-save-history ()
    (when comint-input-ring-file-name
      (when (y-or-n-p "Save this ielm session? ")
        (comint-write-input-ring))))

  (defun pmx-ielm-save-history ()
    (add-hook 'kill-buffer-hook #'pmx--ielm-save-history nil t))

  (add-hook 'ielm-mode-hook #'pmx-ielm-save-history)

  (defun pmx--ielm-comint-truncate ()
    "When outputs are huge, avoid printing them because it will cause the buffer
to be slow."
    (setq-local comint-max-line-length 1024)
    ;; comint-output-filter-functions is permanent local
    (unless (memq #'comint-truncate-buffer comint-output-filter-functions)
      (push #'comint-truncate-buffer
            comint-output-filter-functions)))
  (add-hook 'ielm-mode-hook #'pmx--ielm-comint-truncate))

(use-package lispy
  :hook ((lisp-mode emacs-lisp-mode))
  :config
  (unbind-key (kbd "M-o") lispy-mode-map) ; lispy....i-forgot
  (unbind-key (kbd "M-j") lispy-mode-map) ; lispy-join
  (unbind-key (kbd "M-i") lispy-mode-map) ; lispy-iedit
  (unbind-key (kbd "x") lispy-mode-map)   ; special-lispy-x
  (unbind-key (kbd "b") lispy-mode-map))  ; special-lispy-back

;; Seeing delimiter balance at all times is pretty useful.
(use-package rainbow-delimiters
  :hook ((lisp-mode emacs-lisp-mode)))

(use-package macrostep) ; macro inspection

(use-package sicp)

(use-package info-colors
  :config
  (set-face-attribute 'Info-quoted nil :inherit 'font-lock-doc-face)
  (set-face-attribute 'info-colors-ref-item-user-option nil :inherit 'font-lock-keyword-face)
  (set-face-attribute 'info-colors-ref-item-type nil :inherit 'font-lock-type-face)
  (set-face-attribute 'info-colors-ref-item-variable nil :inherit 'fixed-pitch :foreground "#F43E87")
  (set-face-attribute 'info-colors-lisp-code-block nil :inherit 'font-lock-comment-face)
  (add-hook 'Info-selection-hook 'info-colors-fontify-node))

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

;; pmx-ielm-this-buffer is a shortcut to open an ielm buffer for introspective
;; hacking on that buffer.  This is often faster than the scratch buffer
;; workflow, which both accumulates broken forms and requires manually
;; specifying which buffer you want to operate on.

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

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

(use-package package-lint)

;; Lightweight persistent scratch...okay, we made it support multiple files.  No
;; longer lightweight.
(with-eval-after-load 'no-littering

  (defvar pmx--scratch-dir (no-littering-expand-var-file-name "scratch")
    "Where scratches are saved.")

  (defvar pmx-scratch-template "\
;;; %1$s.el --- Scatch Package -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; good luck!
;;; Code:

(message \"Only the future is certain.\")

(provide '%1$s)
;;; %1$s.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (when (featurep 'jinx) (jinx-mode -1))
;; End:"
    "Template for file backed scratch.")

  (setq-default initial-scratch-message
                (format pmx-scratch-template "scratch"))

  (unless (file-directory-p pmx--scratch-dir)
    (make-directory pmx--scratch-dir))

  (defun pmx--scratch-message (buffer)
    "A motivational passage to start your package."
    (switch-to-buffer buffer)
    (insert (format pmx-scratch-template
                    (file-name-base (buffer-file-name buffer)))))

  ;; TODO customize
  (defvar pmx--scratch-init #'pmx--scratch-message
    "Function to run on each new scratch buffer.")

  (defun pmx--scratch-file-name (buffer-name)
    "Just find the number."
    (if (string= buffer-name "*scratch*")
        "scratch.el"
      (string-match (rx line-start
                        (literal "*scratch*<")
                        (group numeric)
                        (literal ">")
                        line-end)
                    buffer-name)
      (format "scratch-%s.el" (match-string 1 buffer-name))))

  (defun pmx--scratch-buffer-name (file-name)
    "Just find the number."
    (if (string= file-name "scratch.el")
        "*scratch*"
      (string-match (rx line-start
                        (literal "scratch-")
                        (group numeric)
                        (literal ".el")
                        line-end)
                    file-name)
      (format "*scratch*<%s>" (match-string 1 file-name))))

  (defun pmx--scratch-only-elisp-files (dir)
    "Return just normal elisp files in DIR."
    (let ((match-elisp (rx line-start
                           (+ not-newline)
                           (literal ".el")
                           line-end))
          (match-flycheck (rx line-start
                              (literal "flycheck_")
                              (+ not-newline)
                              (literal ".el"))))
      (save-match-data
        (seq-filter
         (lambda (f) (not (string-match-p match-flycheck f)))
         (directory-files dir nil match-elisp)))))

  (defun pmx--read-scratch ()
    "Read a file in the scratch dir if there are files."
    (when-let* ((_ (file-directory-p pmx--scratch-dir))
                (files (pmx--scratch-only-elisp-files pmx--scratch-dir)))
      (completing-read "Choose existing scratch file: " files nil t)))

  (defun pmx-scratch (&optional file-name)
    "Open an old scratch FILE-NAME."
    (interactive (list (pmx--read-scratch)))
    (if file-name
        (let* ((name (pmx--scratch-buffer-name file-name))
               (file-path (expand-file-name file-name pmx--scratch-dir))
               (buffer (find-file-noselect file-path)))
          (unless (string= (buffer-name buffer) name)
            (rename-buffer name))
          (switch-to-buffer buffer))
      (pmx-scratch-new)))

  (defun pmx-scratch-new ()
    "Use a regular file-backed scratch.
It doesn't look like a file-backed buffer, but it is file backed, so Emacs will
ask if you want to save it and other default file behaviors.

TODO make proper well-formed package so that byte compiling and flycheck just
work."
    (interactive)
    (let* ((name (generate-new-buffer-name "*scratch*"))
           (file-name (pmx--scratch-file-name name))
           (buffer (get-buffer-create name)))
      (switch-to-buffer buffer)
      (set-visited-file-name (no-littering-expand-var-file-name
                              (format  "scratch/%s" file-name)))
      (rename-buffer name)
      (funcall pmx--scratch-init buffer)
      (emacs-lisp-mode)))

  (defalias 'scratch-buffer #'pmx--scratch))

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

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

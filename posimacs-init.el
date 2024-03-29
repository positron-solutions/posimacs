;;; posimacs-init.el --- You know -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Load any packages that affect downstream packages, cleanup loading tricks
;; from early init, and then include other modules of configuration which rely
;; mostly on use-package declarations to properly order downstream
;; initialization.

;;; Code

;; Elpaca bootstrap.  Last updated 2023-5-4
(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref "ab5f7156a681b7205b7235861ab2cdc730550382" ; static version
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Load no-littering just after bootsrapping elpaca.  Otherwise, downstream
;; packages may initialize to weird locations, often resulting in missing
;; history and unwanted file littering
(elpaca no-littering
  (require 'no-littering))

(elpaca-wait) ; ensure elpaca finishes queues before continuing

;; See posimacs-early-init.el for initial replacement.
(defun pmx--elpaca-after-init ()
  "Undo filename handler trick and delete self for fun."

  ;; load user's persisted customize values.
  (load
   (setq custom-file (expand-file-name "customs.el" user-emacs-directory))
   'noerror)

  (setq file-name-handler-alist file-name-handler-alist--old)
  (remove-hook 'elpaca-after-init-hook #'pmx--elpaca-after-init)
  (fmakunbound #'pmx--elpaca-after-init)
  (makunbound 'file-name-handler-alist--old))

(add-hook 'elpaca-after-init-hook #'pmx--elpaca-after-init)

;; Idle garbage collection
(use-package gcmh
  :demand t
  :config
  (add-hook 'elpaca-after-init-hook (lambda () (gcmh-mode 1))))

;;; Load modules.
(let ((posimacs-files '("posimacs-defaults.el"
                        "posimacs-bindings.el"
                        "posimacs-elisp.el"
                        "posimacs-extras.el"
                        "posimacs-minibuffer.el"
                        "posimacs-org.el"
                        "posimacs-prog.el"
                        "posimacs-style.el"
                        "posimacs-vc.el"

                        "jinx/posimacs-jinx.el"
                        "rust/posimacs-rust.el"
                        "vterm/posimacs-terminal.el")))
  (dolist (file-name posimacs-files)
    (load (expand-file-name
           (concat "posimacs/" file-name)
           user-emacs-directory))))

(provide 'posimacs-init)
;;; posimacs-init.el ends here.

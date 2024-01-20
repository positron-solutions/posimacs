;;; posimacs-init.el --- You know -*- lexical-binding: t;no-byte-compile: t-*-

;;; Commentary:
;;
;; Load any packages that affect downstream packages, cleanup loading tricks
;; from early init, and then include other modules of configuration which rely
;; mostly on use-package declarations to properly order downstream
;; initialization.

;;; Code:

;; Elpaca bootstrap.  Last updated 01-02-2024
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
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
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
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
  ;; Handy debugging expressions
  ;; (setq use-package-verbose t)
  (setq use-package-compute-statistics t)
  
  (defun elpaca-update (arg)
    "Fetch, merge, rebuild."
    (interactive "p")
    (let ((current-prefix-arg 4)
          (prefix-arg 4))
      (message "%S" arg)
      (call-interactively #'elpaca-merge)))

  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
;; Load no-littering just after bootsrapping elpaca.  Otherwise, downstream
;; packages may initialize to weird locations, often resulting in missing
;; history and unwanted file littering

(elpaca no-littering
  (require 'no-littering))

(elpaca auto-compile
  (require 'auto-compile)
  ;; Don't load outdated .elc files
  (setopt load-prefer-newer t)
  (setopt auto-compile-display-buffer nil)
  ;; You will still get byte compile and you can open *Warnings* buffer
  (setopt native-comp-async-report-warnings-errors 'silent)
  (auto-compile-on-load-mode 1)
  (auto-compile-on-save-mode 1))

;; Scratch buffers that are useful
(elpaca (scratch-pkgs
         ;; :host github :repo "positron-solutions/scratch-pkgs"
         :repo "~/Desktop/positron/scratch-pkgs/")
  (require 'scratch-pkgs)
  (setopt scratch-pkgs-mode 'elpaca)
  (scratch-pkgs-integrate))

;; TODO evidently Elpaca will strongly order the loading.
(elpaca-wait) ; ensure elpaca finishes queues before continuing

;; Make elpaca work with :tag t
;; https://github.com/progfolio/elpaca/issues/126
(defun +elpaca-recipe-tag-latest (recipe)
  "Translate :tag RECIPE keyword's :latest option."
  ;; Only applies when a recipe has specified :tag :latest.
  (when-let ((tag (plist-get recipe :tag))
             ((eq tag 'latest)))
    (list :depth nil      ; Can't rely on a shallow clone to get the latest tag.
          :pre-build      ; Grab the most recent tag and check it out.
          '(let* ((tag (elpaca-with-process
                           (elpaca-process-call "git" "describe" "--tag" "--abbrev=0" "--always")
                         (if success (string-trim stdout)
                           (error "Unable to find latest tag: %S" stderr)))))
             (elpaca-with-process (elpaca-process-call "git" "checkout" tag)
               (if success (message "checked out tag: %s" tag)
                 (error "Unable to check out tag: %S" stderr))))
          ;; remove our custom value so it does not interfere with the usual value checks.
          :tag nil)))
(push #'+elpaca-recipe-tag-latest elpaca-recipe-functions)

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
  (setopt garbage-collection-messages t)
  (setopt gcmh-high-cons-threshold (* 512 800000))
  (setopt gcmh-low-cons-threshold (* 32 800000))
  (setopt gc-cons-percentage 0.4)
  (add-hook 'elpaca-after-init-hook (lambda () (gcmh-mode 1))))

;;; Load modules.
(let ((posimacs-files '("posimacs-defaults.el"
                        "posimacs-windows.el"
                        "posimacs-ai.el"
                        "posimacs-bindings.el"
                        "posimacs-elisp.el"
                        "posimacs-extras.el"
                        "posimacs-minibuffer.el"
                        "posimacs-org.el"
                        "posimacs-prog.el"
                        "posimacs-style.el"
                        "posimacs-vc.el"
                        "posimacs-terminal.el"

                        "jinx/posimacs-jinx.el"
                        "rust/posimacs-rust.el")))
  (dolist (file-name posimacs-files)
    (load (expand-file-name
           (concat "posimacs/" file-name)
           user-emacs-directory))))

;; TODO check loading order and possibly find a better solution for maintaining
;; the hook position at the head.
(use-package envrc
  :config
  (add-hook 'elpaca-after-init-hook
            (lambda () (envrc-global-mode 1))))

(provide 'posimacs-init)
;;; posimacs-init.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

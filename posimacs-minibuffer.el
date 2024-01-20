;;; posimacs-minibuffer --- The Minibuffer on Steroids

;;; Commentary:
;;
;; A completion framework and sources of suggestions to populate it are the most
;; essential behavior to enable in Emacs.  It makes discovering the rest of
;; Emacs as easy as searching, frequently adding additional context to the
;; results such as displaying hotkeys or docstrings, making all workflows become
;; extremely fast with less time spent reading documentation.
;;

;;; Code:

(use-package swiper                ; the better incremental search
  :bind ("M-s" . swiper))

;; case insensitive matching of filenames
(setq read-file-name-completion-ignore-case t)

(use-package prescient
  :after ivy
  (setopt prescient-history-length 200)
  (setopt prescient-sort-length-enable t))

(use-package ivy-prescient
  :hook ivy-mode
  :after ivy-prescient)

(use-package ivy
  :after (orderless counsel)
  :config
  (ivy-mode)
  ;; Always ignore buffers set in `ivy-ignore-buffers'
  (setq ivy-use-ignore-default t)
  (setq ivy-wrap t)
  (setq ivy-height 12)
  (setq ivy-ignore-buffers
        '("magit[\-:]"
          "*Flycheck"
          "*straight"                   ; not in use
          "*Backtrace*"
          "*Completions*"
          "auto-commit-org-files-log"
          "TAGS"
          "*Compile-log*"
          "*Native-compile-log*"
          "*Async-native-compile-log*"))
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))) ; orderless

  ;; The customization of ivy-initial-inputs-alist is to allow matching anywhere
  ;; rather than the first match being at the start of a suggestion
  (setopt ivy-initial-inputs-alist      ; by default, match anywhere in name
          (quote
           ((counsel-minor . "")
            (counsel-package . "")
            (counsel-org-capture . "")
            (counsel-M-x . "")
            (counsel-describe-function . "")
            (counsel-describe-variable . "")
            (org-refile . "")
            (org-agenda-refile . "")
            (org-capture-refile . "")
            (Man-completion-table . "")
            (woman . ""))))
  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)

  ;; prefer alt key more often
  ;; (define-key ivy-minibuffer-map (kbd "M-n") 'ivy-next-line)
  ;; (define-key ivy-minibuffer-map (kbd "M-p") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-n") nil)
  (define-key ivy-minibuffer-map (kbd "C-p") nil))

(use-package ivy-rich  ; More informative ivy completionsq
  :after (ivy counsel-projectile)
  :config
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package nerd-icons-ivy-rich
  :after ivy-rich
  :config
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

;; Counsel provides many of the completion options for base emacs workflows to ivy
(use-package counsel
  :elpaca (counsel :autoloads t)
  :config
  (letrec ((load-counsel (lambda ()
                           (counsel-mode)
                           (remove-hook 'elpaca-after-init-hook load-counsel))))
    (add-hook 'elpaca-after-init-hook load-counsel))
  (setopt counsel-find-file-at-point t))

;; Git & project tree based searching for files
;; TODO move over with VC stuff
(use-package projectile
  :after magit
  :init (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (push "^target" projectile-globally-ignored-directories)
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-completion-system 'ivy)
  (setq counsel-projectile-switch-project-action 'magit-status)
  (setq projectile-switch-project-action 'magit-status))

;; provides emacs ripgrep integration for counsel-projectil-ripgrep
(use-package projectile-ripgrep
  :after projectile)

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode))

;;; posimacs-minibuffer.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

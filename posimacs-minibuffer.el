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

(use-package swiper ; the better buffer search
  :bind ("M-s" . swiper))

;; case insensitive matching of filenames
(setq read-file-name-completion-ignore-case t)

(use-package amx ; better M-x interface -- integrates with Ivy
  :delight amx-mode
  :config
  (amx-mode t))

(use-package ivy
  :delight ivy-mode
  :config
  (ivy-mode)
  ;; Always ignore buffers set in `ivy-ignore-buffers'
  (setq ivy-use-ignore-default t)
  (setq ivy-wrap t)
  (setq ivy-height 16)
  (setq ivy-ignore-buffers
        '("magit[\-:]"
          "*Flycheck"
          "*straight" ; not in use
          "*Backtrace*"
          "*Completions*"
          "auto-commit-org-files-log"
          "TAGS"
          "*Compile-log*"
          "*Native-compile-log*"
          "*Async-native-compile-log*"))
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))) ; orderless

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial-or-done)

  ;; prefer alt key more often
  (define-key ivy-minibuffer-map (kbd "M-n") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "M-p") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-n") nil)
  (define-key ivy-minibuffer-map (kbd "C-p") nil)

  :custom
  (ivy-use-virtual-buffers t))

(use-package ivy-rich  ; More informative ivy completionsq
  :after (ivy counsel)
  :config
  (ivy-rich-mode +1)
  (ivy-rich-project-root-cache-mode +1))

(use-package all-the-icons-ivy-rich
  :after counsel-projectile
  :init  (all-the-icons-ivy-rich-mode 1))

;; Counsel provides many of the completion options for base emacs workflows to ivy
;; The customization of ivy-initial-inputs-alist is to allow matching anywhere
;; rather than the first match being at the start of a suggestion
(use-package counsel
  :delight
  :config
  (counsel-mode)
  (setq counsel-find-file-at-point t)
  (setq ivy-initial-inputs-alist  ; by default, match anywhere in name
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
          (woman . "")))))

;; Git & project tree based searching for files
;; TODO move over with VC stuff
(use-package projectile
  :after magit
  :delight
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
(use-package projectile-ripgrep)

(use-package counsel-projectile
  ; :after (counsel projectile)
  :config
  (counsel-projectile-mode))

;;; posimacs-minibuffer.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

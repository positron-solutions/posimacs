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
  :bind ("C-s" . swiper)
  :config)

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
  (setq ivy-use-ignore-default 'always)
  (setq ivy-wrap t)
  (setq ivy-ignore-buffers '("\\` " "\\`\\*"))

  (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-partial)

  :custom
  (ivy-use-virtual-buffers t))

(use-package ivy-rich  ; More informative ivy completions
  :after ivy
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package helpful  ; much more useful help buffers
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-key] . helpful-key)
         ("H-h" . helpful-at-point)))

;; Counsel provides many of the completion options for base emacs workflows to ivy
;; The customization of ivy-initial-inputs-alist is to allow matching anywhere
;; rather than the first match being at the start of a suggestion
(use-package counsel
  :delight
  :after (ivy helpful)
  :config
  (counsel-mode)
  (setq ivy-initial-inputs-alist
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
        (quote
         ((counsel-minor . "^+")
          (counsel-package . "^+")
          (counsel-org-capture . "")
          (counsel-M-x . "")
          (counsel-describe-function . "")
          (counsel-describe-variable . "")
          (org-refile . "")
          (org-agenda-refile . "")
          (org-capture-refile . "")
          (Man-completion-table . "^")
          (woman . "^")))))

;; Git & project tree based searching for files
(use-package projectile
  :after magit counsel
  :delight
  :init (setq projectile-completion-system 'ivy)
  :bind-keymap ("C-x p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq counsel-projectile-switch-project-action 'magit-status))

;; provides emacs ripgrep integration for counsel-projectil-ripgrep
(use-package projectile-ripgrep)

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

;; https://github.com/domtronn/all-the-icons.el
;; File & other icons used by company-box, ivy, dired, and other packages to
;; give more visual cues about completion options and lists of items
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :config (add-hook 'after-init-hook 'all-the-icons-ivy-setup))
(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;; posimacs-minibuffer.el ends here

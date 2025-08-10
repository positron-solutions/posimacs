;;; posimacs-style --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Changes in this package affect appearence but try not to affect behavior, a
;; goal that is impossible without modular composing configurations, but I will
;; do something about that elsewhere.

;;; Code:

;; TODO check if fontification was broken with the older method
;; TODO narrow the rule to where it should be
(defun pmx--setup-pretty-lambda ()
  (let ((rules `(("\\<lambda\\>"
                  (0  '(face font-lock-keyword-face display "λ"))))))
    (dolist (m '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-mode))
      (font-lock-add-keywords m rules))
    (set-fontset-font "fontset-default"
                      955 (font-spec :family "Roboto Mono"
                                     :inherit 'default))))

;; 안영하세요
(use-package mule
  :ensure nil
  :config
  (setq default-input-method "korean-hangul")
  (define-key global-map (kbd "C-\\") nil)
  (define-key global-map (kbd "M-`") #'toggle-input-method))

;; Better Korean langauge support
(defun pmx--setup-korean-fonts ()
  "Configure Korean chars to be more 아름답지."
  (set-fontset-font t 'korean-ksc5601
                    (font-spec :family "NanumGothicCoding"
                               :inherit 'default)))

(defun pmx--setup-org-bullet-fonts ()
  "The bullets are not actually Roboto Slab."
  (set-fontset-font "fontset-default"
                    8711 (font-spec :family "Roboto Slab"
                                    :inherit 'default))
  (set-fontset-font "fontset-default"
                    8751 (font-spec :family "Iosevka Nerd Font Mono"
                                    :inherit 'default))
  (set-fontset-font "fontset-default"
                    8750 (font-spec :family "Iosevka Nerd Font Mono"
                                    :inherit 'default))
  (set-fontset-font "fontset-default"
                    963 (font-spec :family "Iosevka Nerd Font Mono"
                                   :inherit 'default))
  (set-fontset-font "fontset-default"
                    #xf05c3 (font-spec :family "Iosevka Nerd Font Mono"
                                       :inherit 'default)))

(defun pmx--setup-fonts (&rest _)
  "Nice fonts for nice people (and robots)."
  ;; Base fonts
  (set-face-attribute 'default nil :family "Roboto Mono" :height 108 :weight 'normal)
  (set-face-attribute 'fixed-pitch-serif nil :family "Roboto Mono")
  (set-face-attribute 'variable-pitch nil  :family "Noto Serif CJK KR")
  (set-face-attribute 'header-line nil
                      :background (face-attribute 'default :background))
  (set-face-attribute 'italic nil
                      :slant 'italic :family "Roboto Mono")

  ;; TODO check if these can be fixed via override chains
  (set-face-attribute 'font-lock-doc-face nil
                      :family "Roboto Mono")
  (set-face-attribute 'font-lock-comment-face nil
                      :family "Roboto Mono")
  (set-face-attribute 'font-lock-keyword-face nil
                      :family "Roboto Mono")

  (set-face-attribute 'info-title-4 nil
                      :family "Roboto Slab"
                      :height 1.5
                      :inherit 'default)
  ;; helpful has a :config expression for helpful-heading
  (set-face-attribute 'info-menu-header nil
                      :family "Roboto Slab"
                      :height 1.5
                      :inherit 'default))

(defun pmx--setup-helpful-face ()
  (set-face-attribute 'helpful-heading nil
                      :family "Roboto Slab"
                      :height 1.5
                      :inherit 'default))

(use-package shortdoc
  :ensure nil
  :config
  (set-face-attribute 'shortdoc-section nil :inherit 'fixed-pitch)
  (set-face-attribute 'shortdoc-heading nil :inherit 'fixed-pitch)
  (set-face-attribute 'shortdoc-heading nil
                      :family "Roboto Slab"
                      :height 1.3
                      :weight 'bold
                      :inherit 'default))

(use-package doom-themes
  :config
  (setq custom-theme-directory (expand-file-name "posimacs"
                                                 user-emacs-directory))
  (defface org-indent '((t :inherit default)) "Placeholder")
  (load-theme 'posimacs-dark t)
  (with-eval-after-load 'helpful
    (pmx--setup-helpful-face))
  (pmx--setup-fonts)
  (let ((pmx-setup-style
         (lambda ()
           (load-theme 'posimacs-dark t)
           (pmx--setup-fonts)
           (pmx--setup-korean-fonts)
           (pmx--setup-org-bullet-fonts)
           (pmx--setup-pretty-lambda)

           ;; Corrects (and improves) org-mode's native fontification.
           (doom-themes-org-config))))
    (if (daemonp)
        (add-hook 'server-after-make-frame-hook pmx-setup-style)
      (add-hook 'elpaca-after-init-hook pmx-setup-style))))

;; nice countdown timers
(use-package champagne
  :commands champagne
  :ensure (champagne
           :type git :host github
           :repo "positron-solutions/champagne")
  :config
  (setopt champagne-alpha-background 100))

;; animated parrots and nyancat whenever you finish org todo's or push code
(use-package parrot
  :ensure  (parrot :host github :repo "positron-solutions/parrot" :files (:defaults "img"))
  :custom
  (parrot-animate 'hide-static)
  (parrot-rotate-animate-after-rotation nil)
  (parrot-num-rotations 10)
  (parrot-type 'nyan)
  (parrot-animate-on-load t)
  (parrot-mode t)) ;; enables the mode

(use-package nerd-icons
  :config
  (setq ielm-prompt (concat (nerd-icons-sucicon "nf-custom-emacs") ": ")))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook dired-mode)

(use-package doom-modeline
  :after (nerd-icons doom-themes)
  :init
  (add-hook 'elpaca-after-init-hook #'doom-modeline-mode)
  :config
  (setopt doom-modeline-percent-position nil)
  (setopt column-number-mode nil)
  (setopt line-number-mode nil)
  (setopt mode-line-position-line-format '(" %3l"))
  (setopt doom-modeline-position-line-format '(" %3l"))
  (setopt doom-modeline-position-column-line-format '("%3l:%3c"))
  (setopt doom-modeline-position-column-format '(" %3C"))
  (setopt doom-modeline-buffer-encoding nil)
  (setopt doom-modeline-major-mode-icon t)
  (setopt doom-modeline-env-version nil)
  (setopt doom-modeline-height 34))

;; https://github.com/tarsius/hl-todo
;; highlight TODO keywords in code
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Gotta keep up with everyone else and their cool dashboards
(use-package dashboard
  :after (nerd-icons doom-themes doom-modeline)
  :config
  ;; top stuff
  (setopt dashboard-startup-banner (concat user-emacs-directory "posimacs/graphics/posimacs-banner.png"))
  (setopt dashboard-set-navigator t)
  (setopt dashboard-set-init-info t)
  (setopt dashboard-center-content t)
  (setq dashboard-banner-logo-title nil)

  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-octicon "nf-oct-mark_github" :height 1.1 :v-adjust 0.0)
            "Github"
            "View Public Repository"
            (lambda (&rest _) (browse-url "https://github.com/positron-solutions/posimacs")))
           (,(nerd-icons-octicon "nf-oct-flame" :height 1.1 :v-adjust 0.0)
            "Bugs"
            "Report an Issue or Request Feature"
            (lambda (&rest _) (browse-url "https://github.com/positron-solutions/posimacs/issues")))
           (,(nerd-icons-faicon "nf-fa-reddit_alien" :height 1.1 :v-adjust 0.0)
            "Reddit"
            "Come to Emacs Subreddit!"
            (lambda (&rest _) (browse-url "https://www.reddit.com/r/emacs/top/?t=month"))))))

  ;; items
  (setopt dashboard-set-heading-icons t)
  (setopt dashboard-set-file-icons nil) ; the icons for agenda are not great
  (setq dashboard-items '((projects . 5)
                          (recents  . 5)
                          (bookmarks . 5)))
  (setopt dashboard-show-shortcuts nil)
  (dashboard-modify-heading-icons '((projects . "star")))
  (setq dashboard-item-names `(("Projects:" .
                                ,(concat (nerd-icons-faicon "nf-fa-star") " Projects:"))))

  ;; footer
  (setopt dashboard-set-footer t)
  (setq dashboard-footer-messages '("Next year we're adding a text editor!"))
  (setopt dashboard-footer-icon (nerd-icons-faicon "nf-fa-coffee"
                                                   :height 1.1
                                                   :v-adjust -0.05
                                                   :face 'font-lock-keyword-face))

  ;; other
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
  (add-hook 'elpaca-after-init-hook #'dashboard-initialize))

(provide 'posimacs-style)
;;; posimacs-style.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

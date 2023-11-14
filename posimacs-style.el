;;; posimacs-doom-themes --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Doom has a nice comprehensive theme system and lots of good starting points.
;; Let's adopt!
;;

;;; Code:

(use-package default-text-scale
  :delight default-text-scale-mode
  :config
  (default-text-scale-mode)) ;C-M-= and C-M-- for larger and smaller text

;; Thanks Oleg Pavliv
;; https://unix.stackexchange.com/questions/30039/emacs-how-to-insert-%CE%BB-instead-of-lambda-in-scheme-mode
(defun pmx-greek-lambda ()
  "Make lambda expressions shorter for easier reading in Lisp code."
  (font-lock-add-keywords
   nil
   `(("\\<lambda\\>"
      (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                ,(make-char 'greek-iso8859-7 107))
                nil))))))

(add-hook 'emacs-lisp-mode-hook 'pmx-greek-lambda)
(add-hook 'inferior-emacs-lisp-mode-hook 'pmx-greek-lambda)

;; https://github.com/domtronn/all-the-icons.el
;; File & other icons used by ivy, dired, and other packages to
;; give more visual cues about completion options and lists of items
(use-package all-the-icons
  :config
  (setq ielm-prompt (concat (all-the-icons-fileicon "emacs") ": "))
  (setq ielm-dynamic-multiline-inputs nil) ; multi-line expression indent nicely
  (setq ielm-header "Inferior Emacs Lisp Mode.  M-x `ielm-change-working-buffer' to hack on some live buffer.\n\n")) ; for real

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; 안영하세요
(use-package mule
  :elpaca nil
  :config
  (setq default-input-method "korean-hangul")
  (define-key global-map (kbd "C-\\") nil)
  (define-key global-map (kbd "s-SPC") #'toggle-input-method))

;; Better Korean langauge support
(defun pmx--setup-korean-fonts ()
  "Configure Korean chars to be more 아름답지."
  (set-fontset-font t 'korean-ksc5601
                    (font-spec :family "Gowun Dodum"
                               :inherit 'default)))

(defun pmx--setup-fonts (&rest _)
  "Nice fonts for nice people (and robots)."
  ;; Base fonts
  (set-face-attribute 'default nil :family "Roboto Mono" :height 132 :weight 'normal)
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
  (set-face-attribute 'info-menu-header nil
                      :family "Roboto Slab"
                      :height 1.5
                      :inherit 'default)
  (set-face-attribute 'helpful-heading nil
                      :family "Roboto Slab"
                      :height 1.3
                      :inherit 'default))

(use-package doom-themes
  ;; TODO see if you can leave the loading order independent
  :after org-modern
  :config
  (setq custom-theme-directory (expand-file-name "posimacs" user-emacs-directory))
  (load-theme 'posimacs-dark t)
  (pmx--setup-fonts)
  (let ((pmx-setup-style
         (lambda ()
           (load-theme 'posimacs-dark t)
           (pmx--setup-fonts)
           (pmx--setup-korean-fonts)
           ;; after org-modern
           (pmx-setup-org-fonts)

           ;; Corrects (and improves) org-mode's native fontification.
           (doom-themes-org-config))))
    (if (daemonp)
        (add-hook 'server-after-make-frame-hook pmx-setup-style)
      (add-hook 'elpaca-after-init-hook pmx-setup-style))))

;; nice countdown timers
(use-package champagne
   :elpaca (champagne
           :type git :host github
           :repo "positron-solutions/champagne"))

;; animated parrots and nyancat whenever you finish org todo's or push code
(use-package parrot
  :elpaca  (parrot :host github :repo "positron-solutions/parrot" :files (:defaults "img"))
  :custom
  (parrot-animate 'hide-static)
  (parrot-rotate-animate-after-rotation nil)
  (parrot-num-rotations 10)
  (parrot-type 'nyan)
  (parrot-animate-on-load t)
  (parrot-mode t)) ;; enables the mode


(use-package nerd-icons)
(use-package doom-modeline
  :after nerd-icons
  :init
  (add-hook 'elpaca-after-init-hook #'doom-modeline-mode)
  :config
  (setopt doom-modeline-buffer-encoding nil)
  (setopt doom-modeline-major-mode-icon t)
  (setopt doom-modeline-env-version nil)
  (setopt doom-modeline-height 34))

;; A slightly more informative scratch buffer
(setq initial-scratch-message ";; Only The Future Is Certain.")

;; https://github.com/Malabarba/beacon
;; When you scroll, the cursor highlights very loudly
(use-package beacon
  :after window
  :delight beacon-mode
  :custom
  (beacon-color "#33DB12")
  (beacon-blink-duration 0.3)
  (beacon-size 30)
  ;; if a move jumps out of the 10x5 rectangle, blinks will happen
  (beacon-blink-when-point-moves-horizontally 10)
  (beacon-blink-when-point-moves-vertically 5)
  (beacon-blink-when-window-changes t)
  (beacon-blink-when-focused t)
  :config
  (defun pmx--beaconator (_)
    "Run beacon on every window state change."
    (beacon-blink-automated))
  (push 'pmx--beaconator window-state-change-functions)
  (beacon-mode))

;; https://github.com/tarsius/hl-todo
;; highlight TODO keywords in code
(use-package hl-todo
  :config
  (global-hl-todo-mode))

;; Gotta keep up with everyone else and their cool dashboards
(use-package dashboard
  :after all-the-icons
  :config

  ;; top stuff
  (setq dashboard-startup-banner (concat user-emacs-directory "posimacs/graphics/posimacs-banner.png"))
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-banner-logo-title nil)

  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
            "Github"
            "View Public Repository"
            (lambda (&rest _) (browse-url "https://github.com/positron-solutions/posimacs")))
           (,(all-the-icons-octicon "flame" :height 1.1 :v-adjust 0.0)
            "Bugs"
            "Report an Issue or Request Feature"
            (lambda (&rest _) (browse-url "https://github.com/positron-solutions/posimacs/issues")))
           (,(all-the-icons-faicon "reddit-alien" :height 1.1 :v-adjust 0.0)
            "Reddit"
            "Come to Emacs Subreddit!"
            (lambda (&rest _) (browse-url "https://www.reddit.com/r/emacs/top/?t=month"))))))

  ;; items
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons nil) ; the icons for agenda are not great
  (setq dashboard-items '((projects . 5)
                          (agenda . 5)))
  (setq dashboard-show-shortcuts nil)
  (dashboard-modify-heading-icons '((projects . "star")
                                    (agenda . "milestone")))
  (setq dashboard-item-names '(("Projects:" .
                                (concat (all-the-icons-octicon "star") " Projects:"))
                               ("Agenda for the coming week:" .
                                (concat  " This week's agenda:" (all-the-icons-fileicon "org")))))

  ;; footer
  (setq dashboard-set-footer t)
  (setq dashboard-footer-messages '("Next year we're adding a text editor!"))
  (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                     :height 1.1
                                                     :v-adjust -0.05
                                                     :face 'font-lock-keyword-face))

  ;; other
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

  (add-hook 'elpaca-after-init-hook #'dashboard-open))

(provide 'posimacs-style)
;;; posimacs-style.el ends here.

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

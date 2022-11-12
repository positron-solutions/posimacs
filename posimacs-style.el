;;; posimacs-doom-themes --- Things are looking up -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Doom has a nice comprehensive theme system and lots of good starting points.
;; Let's adopt!
;;

;;; Code:

(defun pmx--setup-fonts (&rest args)
  "Nice fonts for nice people (and robots)."
  (set-face-attribute 'default nil
                      :font (font-spec :family "Roboto Mono"
                                       :size 20))
  (set-face-attribute 'fixed-pitch-serif nil
                      :font (font-spec :family "Roboto Mono"
                                       :size 20)))

;; XXX believe this can be declared once without resorting to hook
(add-hook 'server-after-make-frame-hook #'pmx--setup-fonts)

;; Make lambda expressions shorter for easier reading in lisp code
;; Thanks Oleg Pavliv
;; https://unix.stackexchange.com/questions/30039/emacs-how-to-insert-%CE%BB-instead-of-lambda-in-scheme-mode
(defun pmx-greek-lambda ()
  (font-lock-add-keywords
   nil
   `(("\\<lambda\\>"
      (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                ,(make-char 'greek-iso8859-7 107))
                nil))))))

(add-hook 'emacs-lisp-mode-hook 'pmx-greek-lambda)

(use-package doom-themes
  :config
  (setq custom-theme-directory (expand-file-name "posimacs" user-emacs-directory))
  (load-theme 'posimacs-dark t)

  (pmx--setup-fonts)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Gotta keep up with everyone else and their cool dashboards
(use-package dashboard
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

  (dashboard-setup-startup-hook))

;;; posimacs-doom-themes.el ends here.

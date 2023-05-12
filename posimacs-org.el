;;; posimacs-org --- Making org mode better out of the box -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuring org mode is often as personal as Emacs itself.  These are some
;; configurations that seem pretty solid choices or are in use for maintaining
;; Positron packages.

;;; Code:

(use-package org
  :after projectile
  :config

  ;; When do we ever want to confirm after explicitly requesting execution?
  (setq org-confirm-babel-evaluate nil)

  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-agenda-show-future-repeats 'next)
  (setq org-agenda-prefer-last-repeat t)
  (setq org-adapt-indentation t)

  ;; https://github.com/bastibe/org-journal/issues/392
  ;; having cycling issues
  (setq org-fold-core-style 'overlays)

  (setq org-mouse-1-follows-link nil)
  (setq org-return-follows-link  t)

  (setq org-startup-folded 'show2levels)

  (setq org-agenda-window-setup 'current-window) ;; don't kill window layout

  ;; load org files from ~/.org/
  (setq org-directory "~/.org")
  (setq org-agenda-files (list org-directory))
  (setq org-archive-location "~/.org/archive.org::* From %s")

  ;; orphan captures will go here
  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; correct editing at the end of unexpanded text
  (setq org-catch-invisible-edits 'show-and-error)

  ;; show filename at head of refile suggestions
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil) ;; give ivy all suggestions
  ;; move to file itself for top-level
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-log-done 'time)

  ;; don't let ancestor TODO's be toggled unless all descendents are marked done
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-archive-reversed-order t) ;; newest first in archive


  ;; Set timer to save org buffers and write a commit at midnight
  (defun pmx-commit-agenda-files ()
    (if-let ((git-bin (executable-find "git"))
             (out "auto-commit-org-files-log"))
        (->> (org-agenda-files)
             (-map 'file-name-directory)
             (--map (vc-call-backend (vc-responsible-backend it) 'root it))
             (-distinct)
             (-map (lambda (r)
                     (shell-command
                      (format "cd %s && %s add -A" r git-bin) out out) r))
             (-map (lambda (r)
                     (shell-command
                      (format "cd %s && %s commit -m '%s'" r git-bin
                              (format "%s automatic-commit"
                                      (format-time-string "%Y-%m-%d" (current-time)))) out out))))
      (error "Could not find git executable")))

  (run-at-time "24:00" t (lambda (&rest _args)
                           (pmx-commit-agenda-files))))

;; see agenda whenever you come back from lunch
(use-package idle-org-agenda
  :config
  (setq idle-org-agenda-interval 900)
  (idle-org-agenda-mode))

;; just get the basics out of the way
(use-package org-modern
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom ((org-modern-todo t)
           (org-modern-table nil)
           (org-modern-variable-pitch nil)
           (org-modern-block-fringe nil))
  :commands (org-modern-mode org-modern-agenda)
  :init (global-org-modern-mode))

;; for regenerating toc's in Posimacs packages
(use-package org-make-toc)

;; A pure library for manipulating & consuming org buffers.  This is really
;; useful for org hacking, but you can also use the built-in API's with the
;; benefit of deeper integration with the rest of org.
(use-package org-ml)

(provide 'posimacs-org)
;;; posimacs-org.el ends here

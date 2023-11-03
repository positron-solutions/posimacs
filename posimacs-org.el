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
  (setq org-adapt-indentation nil)

  ;; https://github.com/bastibe/org-journal/issues/392
  ;; having cycling issues
  ;; (setq org-fold-core-style 'overlays)

  (setq org-mouse-1-follows-link nil)
  (setq org-return-follows-link t)

  (setq org-startup-folded 'show2levels)
  (setq org-list-indent-offset 2)             ; bullet lists
  (setq org-insert-heading-respect-content t) ; don't let me screw up structure
  (setq org-agenda-window-setup 'current-window) ; don't kill window layout
  (setq org-auto-align-tags nil)                 ; whitespace is the devil

  ;; load org files from ~/.org/
  (setq org-directory "~/.org")
  (setq org-agenda-files (list org-directory))
  (setq org-archive-location "~/.org/archive.org::* From %s")

  ;; TODO force captures to go somewhere
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
  (setq org-habit-show-habits-only-for-today t)
  (setq org-agenda-show-future-repeats nil)
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . auto)))
  (setq org-cycle-separator-lines 2)

  ;; export settings
  (setq org-html-postamble nil)
  (setq org-html-doctype "html5")
  (setq org-hide-emphasis-markers t)
  (setq org-hide-drawer-startup t)


  ;; variable is configured, but I only use it sometimes manually
  ;; (add-hook 'org-mode-hook #'variable-pitch-mode)
  ;; line spacing
  (add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.6)))

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

;; show things when cursor gets there
(use-package org-appear
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'always)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t)
  (add-hook 'org-mode-hook #'org-appear-mode))


(defun pmx-setup-org-fonts ()
  "Org mode needs it's font's set up after every document load."
  (interactive)
  (set-face-attribute 'org-level-8 nil
                      :inherit 'default
                      :family "Roboto Slab")

  ;; These are not getting set after the doc is in org modern.
  (set-face-attribute 'org-modern-label nil
                      :inherit 'default
                      :family "Roboto Condensed")
  (set-face-attribute 'org-document-info-keyword nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'org-special-keyword nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'org-document-info nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'org-meta-line nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'org-block-begin-line nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR")
  (set-face-attribute 'org-drawer nil
                      :inherit 'default
                      :family "Noto Sans Mono CJK KR"))

;; just get the basics out of the way
(use-package org-modern
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda))
  :custom
  ((org-modern-todo t)
   (org-modern-table nil)
   (org-modern-variable-pitch t)
   (org-modern-hide-stars 'leading)

   ;; I like to change up my stars
   (org-modern-star '("∯" "∮" "∇" "Σ" "∞" "∴"))
   ;; (org-modern-star '("♨" "✈" "✲" "⍟" "✰" "▻" "▸")
   ;; (org-modern-star '("◩" "⬙" "◲" "◇" "▿"))
   ;; (org-modern-star '("█" "▟" "▞" "▄" "▘" "▗" "▁"))
   ;; (org-modern-star '("☀" "☉" "☾" "☄" "❉" "✵" "✰" "✩"))
   ;; (org-modern-star '("∀" "∋" "∃" "≡" "∩"))
   ;; (org-modern-star '("🂡" "🂼" "🃇" "🃋" "🃕" "🂿" "🃒"))
   ;; (org-modern-star '("☰" "☲" "☵" "☷" "⚌" "⚏" "⚋"))


   (org-modern-block-fringe t))
  :commands (org-modern-mode org-modern-agenda)
  :config
  (add-hook 'org-modern-mode-hook #'pmx-setup-org-fonts)
  :init
  (global-org-modern-mode))


;; for regenerating tocs (deprecated in favor of export TOCs)
(use-package org-make-toc)

(use-package visual-fill-column
  :after visual-fill-column
  :config
  (setq visual-fill-column-adjust-for-text-scale nil)
  (setq visual-fill-column-width 120)
  (setq visual-fill-column-center-text t))

(use-package hide-mode-line)

(defun pmx-screencap-setup ()
  "Make it good for video."
  (interactive)
  ;; I was going to praise this function until I needed the 16 pixel width
  ;; correction lol.  Stop wasting my time.
  (when (frame-parameter (selected-frame) 'fullscreen)
    (set-frame-parameter (selected-frame) 'fullscreen nil))
  (set-frame-size (selected-frame) (- 2560 16) 1440 t))


(defvar-local pmx-hide-org-meta-line-cookie nil)

(defun pmx-hide-org-meta-line ()
  "Hide meta keywords."
  (interactive)
  (unless pmx-hide-org-meta-line-cookie
    (setq pmx-hide-org-meta-line-cookie
          (face-remap-add-relative 'org-meta-line :foreground
                                   (face-attribute 'org-meta-line :background)))))

(defun pmx-show-org-meta-line ()
  "Show meta keywords."
  (interactive)
  (when pmx-hide-org-meta-line-cookie
    (face-remap-remove-relative pmx-hid-org-meta-line-cookie)))

(defun pmx-toggle-org-meta-line ()
  "Toggle visibility of meta lines."
  (interactive)
  (if pmx-meta-line-hidden
      (pmx-show-org-meta-line)
    (pmx-hide-org-meta-line)))

(defvar-local pmx-presentation-text-scale 2)
(defvar-local pmx-presentation-full-screen t
  "Set this within a buffer in order to ask for full-screen display.")
(defvar-local pmx-old-window-configuration nil "Restore after tree slide exit.")
(defvar-local pmx--header-line-cookie nil "Restore header line face.")

(defun pmx-presentation-start ()
  "Make tree slide nice."
  (setq-local blink-cursor-alist '(((hbar . 0) . bar)))
  (setq-local cursor-type '(hbar . 0))
  (setq-local blink-cursor-blinks 4)
  (setq-local pmx--header-line-cookie (face-remap-add-relative 'header-line :height 2.0))
  (setq header-line-format " ")
  (org-appear-mode 1)
  (pmx-hide-org-meta-line)
  (text-scale-set pmx-presentation-text-scale)
  (org-display-inline-images nil t (point-min) (point-max))
  (visual-line-mode 1)
  (when pmx-presentation-full-screen
    (fringe-mode '(0 . 0))
    (setq pmx-old-window-configuration (current-window-configuration))
    (delete-other-windows)
    (visual-fill-column-mode 1))
  (git-gutter-mode -1)
  (when (featurep 'jinx)
    (jinx-mode -1))
  ;; claims to be deprecated
  (read-only-mode 1)
  (hide-mode-line-mode 1))

(defun pmx-presentation-stop ()
  "Make tree slide nice."
  (hide-mode-line-mode -1)
  (visual-line-mode -1)
  (when pmx-presentation-full-screen
    (visual-fill-column-mode -1)
    (fringe-mode nil)
    (when pmx-old-window-configuration
      (set-window-configuration pmx-old-window-configuration)
      (setq pmx-old-window-configuration nil)))
  (read-only-mode -1)
  (git-gutter-mode 1)
  (org-display-inline-images nil t (point-min) (point-max))
  (text-scale-set 0)
  (when (featurep 'jinx)
    (jinx-mode 1))
  (pmx-show-org-meta-line)
  (org-appear-mode -1)
  (face-remap-remove-relative pmx--header-line-cookie)
  (setq header-line-format nil)

  (setq-local blink-cursor-alist (default-value 'blink-cursor-alist))
  (setq-local cursor-type (default-value 'cursor-type))
  (setq-local blink-cursor-blinks (default-value 'blink-cursor-blinks)))

;; presenting
(use-package org-tree-slide
  :after hide-mode-line
  :config
  (setq org-tree-slide-never-touch-face t)
  (setq org-tree-slide-header t)
  (setq org-tree-slide-slide-in-effect t)
  (setq org-tree-slide-slide-in-blank-lines 4)
  (setq org-tree-slide-content-margin-top 1)
  (setq org-tree-slide-breadcrumbs
        (propertize " 🢒 "
                    ;; 'display '(raise 0.1)
                    'height 0.2
                    'face '(inherit 'org-level-1)))
  (setq org-tree-slide-skip-outline-level 0)
  (setq org-tree-slide-fold-subtrees-skipped t)

  (defun pmx-org-tree-slide-quit ()
    "Lol, guys, could we have a quit command? Oh."
    (interactive)
    (org-tree-slide-mode -1))

  (defun pmx-org-tree-slide-toggle-full-screen ()
    "When tree slide mode is active, run's teardown hook and set up again."
    (interactive)
    (when org-tree-slide-mode
      (pmx-presentation-stop))
    (setq-local pmx-presentation-full-screen
                (not pmx-presentation-full-screen))
    (when org-tree-slide-mode
      (pmx-presentation-start)))

  (define-key org-tree-slide-mode-map (kbd "<right>") #'org-tree-slide-move-next-tree)
  (define-key org-tree-slide-mode-map (kbd "<left>") #'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<up>") #'org-tree-slide-content)
  (define-key org-tree-slide-mode-map (kbd "<down>") #'pmx-org-tree-slide-quit)

  ;; hack to clear gutter during presentation
  (add-hook 'org-tree-slide-after-narrow-hook #'git-gutter:clear-gutter)

  (add-hook 'org-tree-slide-play-hook #'pmx-presentation-start)
  (add-hook 'org-tree-slide-stop-hook #'pmx-presentation-stop))

(use-package org-present
  :config
  (defun pmx-org-present-prepare-slide (buffer-name heading)
    ;; Show only top-level headlines
    (org-overview)

    ;; Unfold the current entry
    (org-show-entry)

    ;; Show only direct subheadings of the slide but don't expand them
    (org-show-children))
  (add-hook 'org-present-mode-hook 'pmx-presentation-start)
  (add-hook 'org-present-mode-quit-hook 'pmx-presentation-end)
  (add-hook 'org-present-after-navigate-functions 'pmx-org-present-prepare-slide))

;; A pure library for manipulating & consuming org buffers.  This is really
;; useful for org hacking, but you can also use the built-in API's with the
;; benefit of deeper integration with the rest of org.
(use-package org-ml)

(provide 'posimacs-org)

;;; posimacs-org.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

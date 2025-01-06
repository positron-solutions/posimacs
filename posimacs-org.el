;;; posimacs-org --- Making org mode better out of the box -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuring org mode is often as personal as Emacs itself.  These are some
;; configurations that seem pretty solid choices or are in use for maintaining
;; Positron packages.

;;; Code:

(use-package org
  :defer t
  :init
  ;; probably don't need all the modules
  (setq org-modules '(ol-doi
                      ol-w3m
                      ;; ol-bbdb
                      ol-bibtex
                      ol-docview
                      ;; ol-gnus
                      ol-info
                      ol-irc
                      ;; ol-mhe
                      ;; ol-rmail
                      ol-eww))
  :config

  ;; When do we ever want to confirm after explicitly requesting execution?
  (setopt org-confirm-babel-evaluate nil)

  (setopt org-agenda-skip-scheduled-if-done t)
  (setopt org-habit-show-habits-only-for-today t)
  (setopt org-agenda-show-future-repeats 'next)
  (setopt org-agenda-prefer-last-repeat t)
  (setopt org-adapt-indentation nil)

  (setopt org-mouse-1-follows-link t)
  (setopt org-return-follows-link nil)

  ;; lots of org inputs are not pairs
  (defun pmx--org-no-pairs ()
    (electric-pair-local-mode -1))
  (add-hook 'org-mode-hook #'pmx--org-no-pairs)

  ;; Jesus Christ if you're going to automatically move tags to a far column,
  ;; you need to maintain the colulmn while typing or else the lines will
  ;; reflow.
  (setopt org-tags-column 0)

  (setopt org-startup-folded 'fold)
  (setopt org-list-indent-offset 0)             ; bullet lists
  (setopt org-insert-heading-respect-content t) ; don't let me screw up structure
  (setopt org-agenda-window-setup 'current-window) ; don't kill window layout
  (setopt org-auto-align-tags nil)                 ; whitespace is the devil

  ;; Priority
  (setopt org-priority-lowest 1)
  (setopt org-priority-default 5)
  (setopt org-priority-highest 9)

  ;; Constant activity management
  (setq org-agenda-custom-commands
        '(("r" tags "refile")))

  ;; allow image scaling
  (setopt org-image-actual-width nil)

  ;; load org files from ~/.org/
  (setopt org-directory "~/.org")
  (setopt org-agenda-files (list org-directory))
  (setopt org-archive-location "~/.org/archive.org::* From %s")

  ;; TODO force captures to go somewhere
  ;; orphan captures will go here
  (setopt org-default-notes-file (concat org-directory "/notes.org"))

  ;; correct editing at the end of unexpanded text
  (setopt org-catch-invisible-edits 'show-and-error)

  ;; show filename at head of refile suggestions
  (setopt org-refile-use-outline-path 'file)
  (setopt org-outline-path-complete-in-steps nil) ;; give ivy all suggestions

  ;; move to file itself for top-level
  (setopt org-refile-allow-creating-parent-nodes 'confirm)
  (setopt org-log-done 'time)

  ;; don't let ancestor TODO's be toggled unless all descendents are marked done
  (setopt org-enforce-todo-checkbox-dependencies t)
  (setopt org-enforce-todo-dependencies t)
  (setopt org-archive-reversed-order t) ;; newest first in archive
  (setopt org-habit-show-habits-only-for-today t)
  (setopt org-agenda-show-future-repeats nil)
  (setopt org-blank-before-new-entry '((heading . auto)
                                       (plain-list-item . auto)))
  (setopt org-cycle-separator-lines 2)

  ;; export settings
  (setopt org-html-postamble nil)
  (setopt org-html-doctype "html5")
  (setopt org-hide-emphasis-markers t)
  (setopt org-hide-drawer-startup t)

  ;; variable is configured, but I only use it sometimes manually
  ;; (add-hook 'org-mode-hook #'variable-pitch-mode)
  ;; line spacing
  ;; (add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.6)))

  (keymap-unset org-src-mode-map "C-c '")
  (keymap-set org-src-mode-map "C-c C-c" #'org-edit-src-exit)

  ;; TODO this is really scattered.
  ;; See visual fill mode, the M-q binding, the default width settings in
  ;; defaults.
  ;; Also see adaptive wrap.
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'visual-wrap-prefix-mode)
  (keymap-set org-mode-map "C-k" #'kill-visual-line)

  ;; Set timer to save org buffers and write a commit at midnight
  (defun pmx-commit-agenda-files (&rest _)
    (if-let ((git-bin (executable-find "git"))
             (out "auto-commit-org-files-log"))
        (->> (org-agenda-files)
             (-map 'file-name-directory)
             (--map (vc-call-backend (vc-responsible-backend it) 'root it))
             (-distinct)
             (-map (lambda (r)
                     (shell-command
                      (format "cd %s && %s add -A" r git-bin)
                      out out)
                     (shell-command
                      (format "cd %s && %s commit -m '%s'" r git-bin
                              (format "%s automatic-commit"
                                      (format-time-string "%Y-%m-%d"
                                                          (current-time))))
                      out out))))
      (error "Could not find git executable")))

  (run-at-time "24:00" t #'pmx-commit-agenda-files))

;; show things when cursor gets there
(use-package org-appear
  :ensure (org-appear :autoloads t)
  :hook org-mode
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'always)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t))

(use-package orgit
  :commands (orgit-store-link)
  :ensure (orgit
           :host github
           :repo "magit/orgit"))

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
  :after org
  :hook ((org-mode                 . org-modern-mode)
         (org-agenda-finalize-hook . org-modern-agenda)
         (org-modern-mode . pmx-setup-org-fonts))
  :commands (org-modern-mode org-modern-agenda)
  :config
  (setopt org-modern-todo t)
  (setopt org-modern-table nil)
  (setopt org-modern-variable-pitch t)
  (setopt org-modern-hide-stars 'leading)

  ;; I like to change up my stars
  (setopt org-modern-star 'replace)
  (setopt org-modern-replace-stars "∯∮∇σ𝛼∞𝜺")
  ;; (setopt org-modern-star "♨✈✲⍟✰▻▸")
  ;; (setopt org-modern-star "◩⬙◲◇▿")
  ;; (setopt org-modern-star "█▟▞▄▘▗▁")
  ;; (setopt org-modern-star "☀☉☾☄❉✵✰✩")
  ;; (setopt org-modern-star "∀∋∃≡∩)
  ;; (setopt org-modern-star "🂡🂼🃇🃋🃕🂿🃒")
  ;; (setopt org-modern-star "☰☲☵☷⚌⚏⚋")

  (setopt org-modern-block-fringe t))

;; render nodes at other places
(use-package org-transclusion
  :hook org-mode
  :after org)

;; for regenerating tocs (deprecated in favor of export TOCs)
(use-package org-make-toc
  :after org)

(use-package visual-fill-column
  :config
  (setopt visual-fill-column-adjust-for-text-scale nil)
  ;; width is configured in defaults
  (setopt visual-fill-column-center-text t)
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook #'visual-fill-column-mode)))

(use-package hide-mode-line)

;; Add this to your use-package :config section
(defun pmx-screenshots-dir ()
  (interactive)
  ;; When taking screenshots from a focus buffer, it sets
  ;; `moc-focus-base-buffer' so that you can decide based on the base
  ;; buffer, not the focus buffer, which has no associated file.
  (with-current-buffer (or moc-focus-base-buffer
                           (current-buffer))
    (expand-file-name
     "screenshots/"
     (or (when-let ((p (project-current))) (project-root p))
         (temporary-file-directory)))))

(defun pmx--jinx-desc ()
  "Describe jinx mode state."
  (format "jinx %s" (if global-jinx-mode
                         (propertize "on" 'face 'transient-value)
                       (propertize "off" 'face 'shadow))))

(defun pmx--keycast-desc ()
  "Describe keycast mode state."
  (format "keycast %s"
          (if keycast-freestyle-mode
              (propertize "on" 'face 'transient-value)
            (propertize "off" 'face 'shadow))))

(use-package moc
  :ensure (moc :repo "~/.emacs.d/elpaca/repos/moc")
  :after org
  :config
  (transient-append-suffix 'moc-dispatch '(3 2)
    ["Spelling"
     ("j" global-jinx-mode :description pmx--jinx-desc :transient t)])

  (transient-append-suffix 'moc-dispatch '(3 3)
    ["Casting"
     ("k" keycast-freestyle-mode :description pmx--keycast-desc :transient t)])

  (setq moc-subtle-cursor-blinks 2)
  (setq moc-subtle-cursor-interval 0.1)

  ;; (transient-remove-suffix 'moc-dispatch '(3 3))

  ;; configure the function to be called to calculate the correct options at
  ;; runtime
  (setopt moc-screenshot-dir #'pmx-screenshots-dir)

  (defun pmx--no-show-paren ()
    (show-paren-local-mode -1))
  (add-hook 'moc-focus-mode-hook #'pmx--no-show-paren)

  (setopt moc-screenshot-path #'pmx-screenshots-dir))

(use-package coke
  :ensure (coke :repo "~/.emacs.d/etc/scratch-pkgs/coke.git")
  :after org
  :config
  (add-hook 'org-mode-hook #'coke-mode))

(defvar-local pmx-old-window-configuration nil "Restore after tree slide exit.")
(defvar-local pmx--header-line-cookie nil "Restore header line face.")

(use-package macro-slides
  :elpaca (macro-slides :repo "~/.emacs.d/etc/scratch-pkgs/macro-slides.git")
  :after moc
  :config
  (setq ms-breadcrumb-face
        '(:height 240))

  (add-hook 'ms-mode-hook #'moc-present-mode)

  ;; hack to clear gutter during presentation
  (add-hook 'ms-narrow-hook #'git-gutter:clear-gutter)
  (add-hook 'ms-narrow-hook #'moc-hide-refresh))

(use-package unfill
  :after org
  :bind (:map org-mode-map ("M-q" . #'unfill-paragraph)))

;; presenting
;; (use-package org-tree-slide
;;   :after hide-mode-line
;;   :elpaca (org-tree-slide :autoloads t)
;;   :commands (org-tree-slide-mode)
;;   :config
;;   (setopt org-tree-slide-never-touch-face t)
;;   (setopt org-tree-slide-header t)
;;   (setopt org-tree-slide-date nil)
;;   (setopt org-tree-slide-slide-in-effect t)
;;   (setopt org-tree-slide-slide-in-blank-lines 4)
;;   (setopt org-tree-slide-content-margin-top 1)
;;   (setopt org-tree-slide-header-date nil)
;;   (setopt org-tree-slide-header-author nil)
;;   (setopt org-tree-slide-header-email nil)
;;   (setopt org-tree-slide-indicator
;;           '(:next nil :previous nil :content nil))

;;   ;; TODO brutish
;;   (defun pmx-org-tree-slide-toggle-full-screen ()
;;     "When tree slide mode is active, run's teardown hook and set up again."
;;     (interactive)
;;     (when org-tree-slide-mode
;;       (org-tree-slide-stop))
;;     (setq-local pmx-presentation-full-screen
;;                 (not pmx-presentation-full-screen))
;;     (when org-tree-slide-mode
;;       (pmx-presentation-start)))

;;   (define-key org-tree-slide-mode-map (kbd "<right>") #'org-tree-slide-move-next-tree)
;;   (define-key org-tree-slide-mode-map (kbd "<left>") #'org-tree-slide-move-previous-tree)
;;   (define-key org-tree-slide-mode-map (kbd "<up>") #'org-tree-slide-content)
;;   (define-key org-tree-slide-mode-map (kbd "<down>") #'pmx-org-tree-slide-quit)

;;   ;; hack to clear gutter during presentation
;;   (add-hook 'org-tree-slide-after-narrow-hook #'git-gutter:clear-gutter)
;;   (add-hook 'org-tree-slide-before-move-next-hook #'moc-unhide-notes)
;;   (add-hook 'org-tree-slide-before-move-prev-hook #'moc-unhide-notes)
;;   (add-hook 'org-tree-slide-after-narrow-hook #'moc-hide-all)

;;   (add-hook 'org-tree-slide-play-hook #'pmx-presentation-start)
;;   (add-hook 'org-tree-slide-stop-hook #'pmx-presentation-stop)
;;   )

(use-package org-present
  :after org
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
(use-package org-ml
  :after org)

(provide 'posimacs-org)

;;; posimacs-org.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp)
;; eval: (jinx-mode -1)
;; End:

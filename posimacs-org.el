;;; posimacs-org --- Making org mode better out of the box -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Configuring org mode is often as personal as Emacs itself.  These are some
;; configurations that seem pretty solid choices or are in use for maintaining
;; Positron packages.

;;; Code:

(use-package org
  :defer t
  :config

  ;; When do we ever want to confirm after explicitly requesting execution?
  (setopt org-confirm-babel-evaluate nil)

  (setopt org-agenda-skip-scheduled-if-done t)
  (setopt org-habit-show-habits-only-for-today t)
  (setopt org-agenda-show-future-repeats 'next)
  (setopt org-agenda-prefer-last-repeat t)
  (setopt org-adapt-indentation nil)

  ;; https://github.com/bastibe/org-journal/issues/392
  ;; having cycling issues
  ;; (setopt org-fold-core-style 'overlays)

  (setopt org-mouse-1-follows-link t)
  (setopt org-return-follows-link nil)

  ;; Jesus Christ if you're going to automatically move tags to a far column,
  ;; you need to maintain the colulmn while typing or else the lines will
  ;; reflow.
  (setopt org-tags-column 0)

  (setopt org-startup-folded 'show2levels)
  (setopt org-list-indent-offset 0)             ; bullet lists
  (setopt org-insert-heading-respect-content t) ; don't let me screw up structure
  (setopt org-agenda-window-setup 'current-window) ; don't kill window layout
  (setopt org-auto-align-tags nil)                 ; whitespace is the devil

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
  (setopt org-blank-before-new-entry '((heading . t)
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

;; works with `visual-line-mode'
(use-package adaptive-wrap
  :after org
  :config
  (add-hook 'org-mode-hook #'adaptive-wrap-prefix-mode))

;; see agenda whenever you come back from lunch
(use-package idle-org-agenda
  :after org
  :defer 10
  :config
  (setq idle-org-agenda-interval 900)
  (idle-org-agenda-mode))

;; show things when cursor gets there
(use-package org-appear
  :elpaca (org-appear :autoloads t)
  :hook org-mode
  :config
  (setq org-appear-autolinks t)
  (setq org-appear-trigger 'always)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autoentities t))

(use-package orgit
  :commands (orgit-store-link)
  :elpaca (orgit
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
  (setopt org-modern-star '("∯" "∮" "∇" "σ" "𝛼" "∞" "𝜺"))
  ;; (setopt org-modern-star '("♨" "✈" "✲" "⍟" "✰" "▻" "▸")
  ;; (setopt org-modern-star '("◩" "⬙" "◲" "◇" "▿"))
  ;; (setopt org-modern-star '("█" "▟" "▞" "▄" "▘" "▗" "▁"))
  ;; (setopt org-modern-star '("☀" "☉" "☾" "☄" "❉" "✵" "✰" "✩"))
  ;; (setopt org-modern-star '("∀" "∋" "∃" "≡" "∩"))
  ;; (setopt org-modern-star '("🂡" "🂼" "🃇" "🃋" "🃕" "🂿" "🃒"))
  ;; (setopt org-modern-star '("☰" "☲" "☵" "☷" "⚌" "⚏" "⚋"))

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

(use-package moc
  :after org)

(use-package org-coke
  :after org
  :config
  (defun org-coke-after-change-setup ()
    (add-to-list 'after-change-functions
                 #'org-coke-keep-spacing))
  (add-hook 'org-mode-hook #'org-coke-space-out)
  (add-hook 'org-mode-hook #'org-coke-after-change-setup))

(use-package screencap)

(defvar-local pmx-subtle-cursor-active nil)

(defun pmx-toggle-subtle-cursor ()
  "Turn subtle cursor on or off."
  (interactive)
  (if pmx-subtle-cursor-active
      (progn  (setq-local blink-cursor-alist (default-value 'blink-cursor-alist))
              (setq-local cursor-type (default-value 'cursor-type))
              (setq-local blink-cursor-blinks (default-value 'blink-cursor-blinks)))
    (setq-local blink-cursor-alist '(((hbar . 0) . bar)))
    (setq-local cursor-type '(hbar . 0))
    (setq-local blink-cursor-blinks 4))
  (setq-local pmx-subtle-cursor-active (not pmx-subtle-cursor-active)))

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
    (face-remap-remove-relative pmx-hide-org-meta-line-cookie)))

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
  (pmx-toggle-subtle-cursor)
  (setq-local pmx--header-line-cookie (face-remap-add-relative 'header-line :height 2.0))
  (setq header-line-format " ")
  (org-appear-mode -1)
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
  (org-appear-mode 1)
  (face-remap-remove-relative pmx--header-line-cookie)
  (setq header-line-format nil)
  (pmx-toggle-subtle-cursor)
  (setq-local blink-cursor-alist (default-value 'blink-cursor-alist))
  (setq-local cursor-type (default-value 'cursor-type))
  (setq-local blink-cursor-blinks (default-value 'blink-cursor-blinks)))

(use-package unfill
  :after org
  :bind (:map org-mode-map ("M-q" . #'unfill-paragraph)))

;; presenting
(use-package org-tree-slide
  :after hide-mode-line
  :elpaca (org-tree-slide :autoloads t)
  :commands (org-tree-slide-mode)
  :config
  (setopt org-tree-slide-never-touch-face t)
  (setopt org-tree-slide-header t)
  (setopt org-tree-slide-date nil)
  (setopt org-tree-slide-slide-in-effect t)
  (setopt org-tree-slide-slide-in-blank-lines 4)
  (setopt org-tree-slide-content-margin-top 1)
  (setopt org-tree-slide-activate-message "")
  (setopt org-tree-slide-indicator
          '(:next nil :previous nil :content nil))
  (setopt org-tree-slide-breadcrumbs
          (propertize " 🢒 "
                      ;; 'display '(raise 0.1)
                      'height 0.2
                      'face '(inherit 'org-level-1)))
  (setopt org-tree-slide-skip-outline-level 0)
  (setopt org-tree-slide-fold-subtrees-skipped nil)

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
  (add-hook 'org-tree-slide-before-move-next-hook #'moc-unhide-notes)
  (add-hook 'org-tree-slide-before-move-prev-hook #'moc-unhide-notes)
  (add-hook 'org-tree-slide-after-narrow-hook #'moc-hide-notes)

  (add-hook 'org-tree-slide-play-hook #'pmx-presentation-start)
  (add-hook 'org-tree-slide-stop-hook #'pmx-presentation-stop))

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

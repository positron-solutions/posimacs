;;; posimacs-dark-theme.el --- Posimacs dark theme -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;; Variables
(defgroup posimacs-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom posimacs-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'posimacs-dark-theme
  :type 'boolean)

(defcustom posimacs-dark-brighter-comments nil
  "If non-nil, comments will be highlightzed in more vivid colors."
  :group 'posimacs-dark-theme
  :type 'boolean)

(defcustom posimacs-dark-comment-bg posimacs-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'posimacs-dark-theme
  :type 'boolean)

(defcustom posimacs-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'posimacs-dark-theme
  :type '(or integer boolean))

;;; Theme definition
(def-doom-theme posimacs-dark
  "A dark theme inspired by Acario dark, but with positive vibes"

;;;; Colors
  ;; name        default   256         16
  ((bg         '("#0D0E16" "color-233" "black"        ))
   (bg-alt     '("#040408" "color-232" "brightblack"  ))
   (widget     '("#1F2133" "color-232" "brightblack"  ))
   (base0      '("#0F1019" "color-234" "black"        ))
   (base1      '("#121212" "color-233" "brightblack"  ))
   (base2      '("#28233B" "color-236" "brightblack"  ))
   (base3      '("#464A56" "color-240" "brightblack"  ))
   (base4      '("#585C6C" "color-60"  "brightblack"  ))
   (base5      '("#767676" "color-243" "brightblack"  ))
   (base6      '("#959EA5" "color-109" "white"        ))
   (base7      '("#B2B2B2" "color-249" "white"        ))
   (base8      '("#D0D0D0" "color-252" "brightwhite"  ))
   (fg         '("#CEDBE5" "color-152" "brightwhite"  ))
   (fg-alt     '("#E5F4FF" "color-195" "brightwhite"  ))

   (grey       base5)

   (tooltip-bg '("#12141F" "color-017" "brightblack"  ))
   (tooltip-hl '("#1B1E2E" "color-018" "white"        ))

   (red        '("#FF293A" "color-167" "red"          ))
   (neon       '("#F43E87" "color-207" "brightmagenta"))
   (lneon      '("#FF599C" "color-170" "brightmagenta"))
   (dneon      '("#330D1C" "color-052" "brightblack"  ))
   (ddneon     '("#2E0214" "color-052" "brightblack"  ))
   (green      '("#6EC248" "color-113" "green"        ))
   (lime       '("#33DB12" "color-46"  "lightgreen"   ))
   (llime      '("#4FFF9B" "color-48"  "lightgreen"   ))
   (dlime      '("#061A02" "color-22"  "brightblack"  ))
   (ddlime     '("#051402" "color-22"  "brightblack"  ))

   (yellow     '("#FFD000" "color-179" "brightyellow" ))
   (blue       '("#51A2E0" "color-117" "blue"         ))
   (spark      '("#1983D4" "color-117" "blue"         ))
   (dark-spark '("#0D446E" "color-75"  "blue"         ))
   (light-spark'("#90BDE0" "color-75"  "brightblue"   ))
   (shock      '("#FF6933" "color-202" "yellow"       ))
   (cyan       '("#36D8BD" "color-79"  "cyan"         ))
   (raw        '("#B272C4" "color-134" "magenta"      ))
   (chill      '("#0CC74A" "color-121" "green"        ))
   (ill        '("#1D823E" "color-29"  "green"        ))

   (orange     '("#D85F00" "color-166" "yellow"       ))
   (teal       '("#2D9574" "color-29"  "brightcyan"   ))
   (magenta    '("#8041D8" "color-98"  "magenta"      ))
   (match2     '("#8845E6" "color-98" "magenta"       ))
   (dmatch2    '("#0F081A" "color-17" "magenta"       ))
   (violet     '("#AB11D8" "color-128" "magenta"      ))

   (bg-hl      '("#143666" "color-19"  "brightblack"  ))
   (bg-blue    '("#0C213E" "color-17"  "brightblack"  ))
   (dark-blue  bg-blue)
   (bg-cyan    '("#092D27" "color-23"  "brightblack"  ))
   (dark-cyan  bg-cyan)

;;;; face categories -- required for all themes
   (highlight      orange)
   (vertical-bar   widget)
   (selection      bg-hl)
   (builtin        blue)
   (comments       (if posimacs-dark-brighter-comments bg-cyan grey))
   (doc-comments   (if posimacs-dark-brighter-comments chill raw))
   (constants      shock)
   (functions      yellow)
   (keywords       lneon)
   (methods        cyan)
   (operators      blue)
   (type           blue)
   (strings        raw)
   (variables      (doom-lighten cyan 0.4))
   (numbers        orange)
   (region         base2)
   (error          red)
   (warning        orange)
   (success        lime)
   (vc-modified    yellow)
   (vc-added       lime)
   (vc-deleted     red)

;;;; custom categories
   (hidden bg)
   (-modeline-bright posimacs-dark-brighter-modeline)
   (-modeline-pad
    (when posimacs-dark-padded-modeline
      (if (integerp posimacs-dark-padded-modeline) posimacs-dark-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base7)

   (modeline-bg
    (if -modeline-bright
        (doom-blend blue bg-alt 0.35)
      `(,(car base2) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        modeline-bg
      `(,(car base3) ,@(cdr base1))))
   (modeline-bg-inactive   (doom-darken bg 0.20))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.2) ,@(cdr base0))))

;;;; --- extra faces ------------------------

  (((all-the-icons-dblue &override) :foreground teal)
   (elscreen-tab-other-screen-face :background bg-blue :foreground fg-alt)

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

;;;;; hl-fill-column-face
   (hl-fill-column-face :background bg-alt :foreground fg-alt)

;;;;; line-number
   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :bold bold)

;;;;; comments and doc
   (font-lock-comment-face
    :inherit 'fixed-pitch-serif
    :slant 'italic
    :foreground comments
    :background (if posimacs-dark-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

;;;;; Flycheck
   (flycheck-popup-tip-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-info-face :background bg-blue :foreground fg-alt)
   (flycheck-posframe-warning-face :inherit 'warning)
   (flycheck-posframe-error-face :inherit 'error)

;;;;; Magit
   (magit-blame-culprit :foreground yellow)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground yellow)
   (magit-blame-subject :foreground yellow)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground yellow)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground yellow)
   (magit-blame-summary :foreground yellow)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground shock)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground shock)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :foreground neon)
   (magit-branch :foreground shock :weight 'bold)
   (magit-branch-current :foreground blue :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground orange :weight 'bold)
   (magit-diff-file-header :foreground blue)
   (magit-diff-file-heading :foreground spark :weight 'light)
   (magit-diff-file-heading-highlight :foreground blue :weight 'bold)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background base1)
   (magit-diff-hunk-heading :foreground spark :weight 'light)
   (magit-diff-hunk-heading-highlight :foreground spark :weight 'bold)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-added :foreground green :weight 'light :background ddlime)
   (magit-diff-removed :foreground neon :weight 'light :background ddneon)
   (magit-diff-context :foreground fg :weight 'light)
   (magit-diff-added-highlight :foreground green :weight 'bold :background dlime)
   (magit-diff-removed-highlight :foreground neon :weight 'bold :background dneon)
   (magit-diff-context-highlight :foreground fg :weight 'bold)
   (magit-diff-base :foreground fg :weight 'light)
   (magit-diff-base-highlight :foreground fg :weight 'bold)
   (magit-diff-lines-boundary :background fg :foreground base2)
   (magit-diff-lines-heading :background fg :foreground base2)
   (magit-hash :foreground yellow)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground yellow)
   (magit-log-head-label-head :background spark :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background neon :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background shock :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground yellow :weight 'bold)
   (magit-section-heading :foreground neon)
   (magit-section-highlight :weight 'bold)
   (section-heading-selection :foreground neon :weight 'bold)
   (magit-section-title :background bg-alt :foreground neon :weight 'bold)
   (magit-cherry-equivalent :foreground shock)
   (magit-cherry-unmatched :foreground cyan)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-bisect-bad :foreground neon)
   (magit-bisect-good :foreground green)
   (magit-bisect-skip :foreground fg)
   (magit-diff-conflict-heading :foreground fg)
   (magit-dimmed :foreground base8)
   (magithub-ci-no-status :foreground grey)
   (magithub-issue-number :foreground fg)
   (magithub-notification-reason :foreground fg)

;;;;; Modeline, Solaire modeline and Doom modeline
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

;;;;; whitespace
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default)

;;;;; ivy-mode
   (ivy-current-match :background light-spark :foreground bg :distant-foreground spark :weight 'normal)
   (ivy-posframe :background base1 :foreground fg)
   (ivy-minibuffer-match-face-1 :distant-foreground dark-spark :foreground spark)
   (ivy-minibuffer-match-face-2 :distant-foreground dmatch2 :foreground match2)
   (ivy-minibuffer-match-face-3 :distant-foreground ddlime :foreground lime)
   (internal-border :background base7)

;;;;; tooltips for company & lsp ui
   (company-tooltip :background tooltip-bg)
   (internal-border :background dark-spark)
   (tooltips :background tooltip-bg)
   (lsp-ui-doc-background :background tooltip-bg)
;;;;; lsp-mode and lsp-ui-mode
   (lsp-ui-peek-highlight :foreground yellow)
   (lsp-ui-sideline-symbol-info :foreground (doom-blend comments bg 0.85)
                                :background bg-alt)

;;;; --- major-mode faces -------------------
;;;;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

;;;; --- dashboard --------------------------
   (dashboard-banner-logo-title :foreground lime :height 128)

;;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground neon)
   ((markdown-code-face &override) :background (doom-lighten tooltip-bg 0.05))

;;;;; org-mode
   ((org-block &override) :background bg-alt)
   ((org-block-begin-line &override) :background bg :foreground comments :slant 'italic)
   ((org-quote &override) :background base1)
   ((org-done &override) :foreground llime)
   ((org-todo &override) :foreground shock)

   (org-hide :foreground hidden))


  ;;;; --- extra variables ---------------------
  ;; ()
  )

;;; posimacs-dark-theme.el ends here

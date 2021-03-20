;;; posimacs-doom-themes --- Things are looking up

;;; Commentary:
;;
;; Doom has a nice comprehensive theme system and lots of good starting points.
;; Let's adopt!
;;

;;; Code:

(set-face-attribute 'default nil
                    :font (font-spec :family "Roboto Mono"
                                     :size 22))
(set-face-attribute 'fixed-pitch-serif nil
                    :font (font-spec :family "Roboto Mono"
                                     :size 22))

(use-package doom-themes
  ;; XXX Module structure would at most default this behavior
  :config
  (load-theme 'posimacs-dark t)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; posimacs-doom-themes.el ends here.

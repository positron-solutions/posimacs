((elpaca :source "lockfile" :date
         (25687 51434 788959 981000)
         :recipe
         (:protocol https :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "ab5f7156a681b7205b7235861ab2cdc730550382" :files
                    (:defaults
                     (:exclude "extensions"))
                    :build
                    (:not elpaca--activate-package)
                    :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date
                     (25687 51434 781737 262000)
                     :recipe
                     (:package "elpaca-use-package" :repo "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el")
                               :main "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info)
                               :protocol https :inherit t :depth 1 :ref "ab5f7156a681b7205b7235861ab2cdc730550382"))
 (no-littering :source "lockfile" :date
               (25687 51434 771627 19000)
               :recipe
               (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "6a8d50f067cc28cbeafdaa09c16644cb63a97cc0"))
 (compat :source "lockfile" :date
         (25687 51434 759578 887000)
         :recipe
         (:package "compat" :host github :files
                   ("*"
                    (:exclude ".git"))
                   :repo "emacs-straight/compat" :protocol https :inherit t :depth 1 :ref "cd28402790821ce95ea4c3ea0de5c2d9dcfce31f"))
 (gcmh :source "lockfile" :date
       (25687 51434 748616 858000)
       :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (direnv :source "lockfile" :date
         (25687 51434 739693 81000)
         :recipe
         (:package "direnv" :fetcher github :repo "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "268536f564b7eba99264a89a9149268eb4bc67ac"))
 (ws-butler :source "lockfile" :date
            (25687 51434 729945 455000)
            :recipe
            (:package "ws-butler" :fetcher github :repo "lewang/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e3a38d93e01014cd47bf5af4924459bd145fd7c4"))
 (command-log :source "lockfile" :date
              (25687 51434 721688 35000)
              :recipe
              (:protocol https :inherit t :depth 1 :host github :repo "positron-solutions/command-log" :package "command-log" :ref "fd358865e89c46fa2e7e90d655d787e448ab368e"))
 (switch-window :source "lockfile" :date
                (25687 51434 712677 794000)
                :recipe
                (:package "switch-window" :repo "dimitri/switch-window" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "71ef2f54c97f3fd2e7ff7964d82e6562eb6282f7"))
 (rotate :source "lockfile" :date
         (25687 51434 702924 581000)
         :recipe
         (:package "rotate" :fetcher github :repo "daichirata/emacs-rotate" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "4e9ac3ff800880bd9b705794ef0f7c99d72900a6"))
 (avy :source "lockfile" :date
      (25687 51434 693073 589000)
      :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (general :source "lockfile" :date
          (25687 51434 683050 508000)
          :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "7ce8db297e3de258ec43802269438ac7f1918707"))
 (transient :source "lockfile" :date
            (25687 51434 673016 253000)
            :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "af7fe42bd46e24ca7852e73bd1691015c5bd2151"))
 (macrostep :source "lockfile" :date
            (25687 51434 663258 919000)
            :recipe
            (:package "macrostep" :fetcher github :repo "emacsorphanage/macrostep" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "cdaa3a5e8235b6edd2e862eb272710471a82039d"))
 (helpful :source "lockfile" :date
          (25687 51434 654778 495000)
          :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "8893e4ba49e4447b41ee08d93e58c23e07bc7514"))
 (auto-compile :source "lockfile" :date
               (25687 51434 644711 973000)
               :recipe
               (:package "auto-compile" :repo "emacscollective/auto-compile" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "cdb60240000aff141fbe26f1487fbfe499ff64dc"))
 (restart-emacs :source "lockfile" :date
                (25687 51434 634684 422000)
                :recipe
                (:package "restart-emacs" :fetcher github :repo "iqbalansari/restart-emacs" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "1607da2bc657fe05ae01f7fdf26f716eafead02c"))
 (snow :source "lockfile" :date
       (25687 51434 623899 162000)
       :recipe
       (:package "snow" :fetcher github :repo "alphapapa/snow.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :host github :ref "be17977677fa29709a726715a1a1cba1bd299f68"))
 (keyfreq :source "lockfile" :date
          (25687 51434 612639 330000)
          :recipe
          (:package "keyfreq" :fetcher github :repo "dacap/keyfreq" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "dd88193cd7a91a92113121191573758ea2a3ceb1"))
 (vlf :source "lockfile" :date
      (25687 51434 601329 72000)
      :recipe
      (:package "vlf" :repo "m00natic/vlfi" :fetcher github :old-names
                (vlfi)
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c"))
 (sudo-edit :source "lockfile" :date
            (25687 51434 589659 899000)
            :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (swiper :source "lockfile" :date
         (25687 51434 581765 724000)
         :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files
                   ("swiper.el")
                   :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (amx :source "lockfile" :date
      (25687 51434 570965 29000)
      :recipe
      (:package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "5b3aa1aae84f4a225cb8d26ab79a32f97693f023"))
 (ivy :source "lockfile" :date
      (25687 51434 560508 93000)
      :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el"))
                :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (ivy-rich :source "lockfile" :date
           (25687 51434 550269 133000)
           :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (all-the-icons-ivy-rich :source "lockfile" :date
                         (25687 51434 540300 249000)
                         :recipe
                         (:package "all-the-icons-ivy-rich" :fetcher github :repo "seagle0128/all-the-icons-ivy-rich" :files
                                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                   :protocol https :inherit t :depth 1 :ref "c098cc85123a401b0ab8f2afd3a25853e61d7d28"))
 (counsel :source "lockfile" :date
          (25687 51434 529136 588000)
          :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files
                    ("counsel.el")
                    :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (projectile :source "lockfile" :date
             (25687 51434 518076 223000)
             :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "271007c6611fcb08ddd326d7de9727c2ad5ef265"))
 (projectile-ripgrep :source "lockfile" :date
                     (25687 51434 509250 643000)
                     :recipe
                     (:package "projectile-ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
                               ("projectile-ripgrep.el")
                               :protocol https :inherit t :depth 1 :ref "b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a"))
 (counsel-projectile :source "lockfile" :date
                     (25687 51434 499287 67000)
                     :recipe
                     (:package "counsel-projectile" :fetcher github :repo "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :protocol https :inherit t :depth 1 :ref "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (org :source "lockfile" :date
      (25687 51434 488873 224000)
      :recipe
      (:package "org" :local-repo "org" :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :pre-build
                (progn
                  (require 'elpaca-menu-org)
                  (elpaca-menu-org--build))
                :build
                (:not elpaca--generate-autoloads-async)
                :files
                (:defaults
                 ("etc/styles/" "etc/styles/*" "doc/*.texi"))
                :protocol https :inherit t :depth 1 :ref "080710797ad25e76c4556d2b03cc0aa5313cd187"))
 (idle-org-agenda :source "lockfile" :date
                  (25687 51434 479125 737000)
                  :recipe
                  (:package "idle-org-agenda" :fetcher github :repo "enisozgen/idle-org-agenda" :files
                            ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                             (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                            :protocol https :inherit t :depth 1 :ref "8e6052fc4923c30132052d67d794b76c92851c20"))
 (org-modern :source "lockfile" :date
             (25687 51434 468977 361000)
             :recipe
             (:package "org-modern" :repo "minad/org-modern" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "e0a1a00490acc3bbbf199e433c9ef60e03f9d416"))
 (org-make-toc :source "lockfile" :date
               (25687 51434 457703 351000)
               :recipe
               (:package "org-make-toc" :fetcher github :repo "alphapapa/org-make-toc" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa"))
 (org-ml :source "lockfile" :date
         (25687 51434 449622 978000)
         :recipe
         (:package "org-ml" :repo "ndwarshuis/org-ml" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "f57336a9126a168ad32ccce017c072474555395a"))
 (nix-mode :source "lockfile" :date
           (25687 51434 437322 90000)
           :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :protocol https :inherit t :depth 1 :ref "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (list-environment :source "lockfile" :date
                   (25687 51434 427584 730000)
                   :recipe
                   (:package "list-environment" :fetcher github :repo "dgtized/list-environment.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :protocol https :inherit t :depth 1 :ref "0a72a5a9c1abc090b25202a0387e3f766994b053"))
 (lsp-mode :source "lockfile" :date
           (25687 51434 417030 785000)
           :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.el")
                     :protocol https :inherit t :depth 1 :ref "cf30718ed5128753565452f476385dac1f7821d6"))
 (lsp-ui :source "lockfile" :date
         (25687 51434 407263 45000)
         :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources")
                   :protocol https :inherit t :depth 1 :ref "295d8984da06a745b0a36c56e28ce915bc389adb"))
 (company :source "lockfile" :date
          (25687 51434 395814 151000)
          :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small" "doc/images/small/*.png"))
                    :protocol https :inherit t :depth 1 :ref "8a78f320019574bc35b5727f95b052b27918da20"))
 (orderless :source "lockfile" :date
            (25687 51434 389019 490000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
 (yasnippet :source "lockfile" :date
            (25687 51434 378386 344000)
            :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets")
                      :protocol https :inherit t :depth 1 :ref "5cbdbf0d2015540c59ed8ee0fcf4788effdf75b6"))
 (flycheck :source "lockfile" :date
           (25687 51434 368937 220000)
           :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "5f2ef177cb21ae8b73714575802beef04abd0f5e"))
 (exec-path-from-shell :source "lockfile" :date
                       (25687 51434 357394 460000)
                       :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :protocol https :inherit t :depth 1 :ref "ddd24dc823de9a94435b4d8ea7569161657f31e2"))
 (default-text-scale :source "lockfile" :date
                     (25687 51434 350802 60000)
                     :recipe
                     (:package "default-text-scale" :fetcher github :repo "purcell/default-text-scale" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :protocol https :inherit t :depth 1 :ref "bfc0987c37e93742255d3b23d86c17096fda8e7e"))
 (all-the-icons :source "lockfile" :date
                (25687 51434 341705 146000)
                :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github :files
                          (:defaults "data")
                          :protocol https :inherit t :depth 1 :ref "8a7e70da843d99a36ad3c2b9e19924dcf533e5a0"))
 (all-the-icons-dired :source "lockfile" :date
                      (25687 51434 330463 263000)
                      :recipe
                      (:package "all-the-icons-dired" :repo "wyuenho/all-the-icons-dired" :fetcher github :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :protocol https :inherit t :depth 1 :ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a"))
 (doom-themes :source "lockfile" :date
              (25687 51434 322584 523000)
              :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el")
                        :protocol https :inherit t :depth 1 :ref "e4f0b006a516a35f53df2dce2ec116876c5cd7f9"))
 (champagne :source "lockfile" :date
            (25687 51434 314697 611000)
            :recipe
            (:protocol https :inherit t :depth 1 :type git :host github :repo "positron-solutions/champagne" :package "champagne" :ref "af8585907b6085170fad5adc2fbe78ef299e3a42"))
 (parrot :source "lockfile" :date
         (25687 51434 305991 41000)
         :recipe
         (:package "parrot" :fetcher github :repo "positron-solutions/parrot" :files
                   (:defaults "img")
                   :protocol https :inherit t :depth 1 :host github :ref "2d7d61c1d665bc8ab17de941300001c781433ca7"))
 (doom-modeline :source "lockfile" :date
                (25687 51434 296887 4000)
                :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "8d9815186009956cb04c045739d31625d8dac6d5"))
 (beacon :source "lockfile" :date
         (25687 51434 286964 285000)
         :recipe
         (:package "beacon" :fetcher github :repo "Malabarba/beacon" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "85261a928ae0ec3b41e639f05291ffd6bf7c231c"))
 (hl-todo :source "lockfile" :date
          (25687 51434 278850 808000)
          :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "b27cddf7373408681cc949c8ef829f87a01ed3f3"))
 (dashboard :source "lockfile" :date
            (25687 51434 268876 267000)
            :recipe
            (:package "dashboard" :fetcher github :repo "emacs-dashboard/emacs-dashboard" :files
                      (:defaults "banners")
                      :protocol https :inherit t :depth 1 :ref "3fce60c285ed4d22a00d6f5b49335d038aa9cd41"))
 (magit :source "lockfile" :date
        (25687 51434 259442 19000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
                   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (magit-todos :source "lockfile" :date
              (25687 51434 250674 967000)
              :recipe
              (:package "magit-todos" :fetcher github :repo "alphapapa/magit-todos" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "7724259a008144b8cfc6cacdae3e764f207a03e7"))
 (git-messenger :source "lockfile" :date
                (25687 51434 240769 918000)
                :recipe
                (:package "git-messenger" :repo "emacsorphanage/git-messenger" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "eade986ef529aa2dac6944ad61b18de55cee0b76"))
 (git-gutter :source "lockfile" :date
             (25687 51434 230145 433000)
             :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "1451e3149865b88de2402ce4296ee5608fadc5b2"))
 (rustic :source "lockfile" :date
         (25687 51434 219664 541000)
         :recipe
         (:package "rustic" :repo "brotzeit/rustic" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "39423d1cf4fa054c36bf9577356451f4c06ee148"))
 (pos-tip :source "lockfile" :date
          (25687 51434 208981 669000)
          :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "bfe74204d1201a33ace81898e7c485382817510a"))
 (clippy :source "lockfile" :date
         (25687 51434 198819 813000)
         :recipe
         (:package "clippy" :fetcher github :repo "Fuco1/clippy.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "85aec3129ff17f71ea4541cfadbb7b56b31a7474"))
 (nov :source "lockfile" :date
      (25687 51434 189396 810000)
      :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "58c35e677e11f5c04a702b42ac753c80c8955089"))
 (k8s-mode :source "lockfile" :date
           (25687 51434 180169 223000)
           :recipe
           (:package "k8s-mode" :fetcher github :repo "TxGVNN/emacs-k8s-mode" :files
                     ("*.el"
                      ("snippets/k8s-mode" "snippets/k8s-mode/*"))
                     :protocol https :inherit t :depth 1 :ref "83266cecd6a39cdf57d124270646047860bfb7ab"))
 (transient-showcase :source "lockfile" :date
                     (25687 51434 169424 680000)
                     :recipe
                     (:protocol https :inherit t :depth 1 :host github :repo "positron-solutions/transient-showcase" :package "transient-showcase" :ref "d28080d63f82b8c29f2e1ee9774172d1a33325c1"))
 (erk :source "lockfile" :date
      (25687 51434 160715 945000)
      :recipe
      (:package "erk" :fetcher github :repo "~/Desktop/positron/elisp-repo-kit/" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "6ca1aabc6c60c8722bc5dca4aedbc033b1d9fff5"))
 (dash :source "lockfile" :date
       (25687 51434 151008 268000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi")
                 :protocol https :inherit t :depth 1 :ref "96eaba028ac069ea0e5cc70de15b0229126a054a"))
 (s :source "lockfile" :date
    (25687 51434 139760 448000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (f :source "lockfile" :date
    (25687 51434 129079 252000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "af7d37c619010b576fd22b50c62c71ff33093f3c"))
 (elisp-refs :source "lockfile" :date
             (25687 51434 118065 52000)
             :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults
                        (:exclude "elisp-refs-bench.el"))
                       :protocol https :inherit t :depth 1 :ref "bf3cca8f74065b1b31036f461e3a093b162311bd"))
 (ripgrep :source "lockfile" :date
          (25687 51434 107871 768000)
          :recipe
          (:package "ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
                    ("ripgrep.el")
                    :protocol https :inherit t :depth 1 :ref "b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a"))
 (magit-section :source "lockfile" :date
                (25687 51434 98396 104000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi")
                          :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (ht :source "lockfile" :date
     (25687 51434 88233 131000)
     :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "3c1677f1bf2ded2ab07edffb7d17def5d2b5b6f6"))
 (spinner :source "lockfile" :date
          (25687 51434 79698 790000)
          :recipe
          (:package "spinner" :host github :files
                    ("*"
                     (:exclude ".git"))
                    :repo "emacs-straight/spinner" :protocol https :inherit t :depth 1 :ref "634529bb3173e09b37499f636de70abf29d9fa8a"))
 (markdown-mode :source "lockfile" :date
                (25687 51434 71613 948000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "5d98592fe516748034d8baf92d7c0ba045e1f87a"))
 (lv :source "lockfile" :date
     (25687 51434 62317 427000)
     :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el")
               :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (eldoc :source "lockfile" :date
        (25687 51434 50986 636000)
        :recipe
        (:package "eldoc" :host github :files
                  ("*"
                   (:exclude ".git"))
                  :repo "emacs-straight/eldoc" :protocol https :inherit t :depth 1 :ref "bf2e88dcf00e6554e24b517aa315527011042fae"))
 (pkg-info :source "lockfile" :date
           (25687 51434 38314 610000)
           :recipe
           (:package "pkg-info" :repo "emacsorphanage/pkg-info" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "76ba7415480687d05a4353b27fea2ae02b8d9d61"))
 (let-alist :source "lockfile" :date
            (25687 51434 29314 706000)
            :recipe
            (:package "let-alist" :host github :files
                      ("*"
                       (:exclude ".git"))
                      :repo "emacs-straight/let-alist" :protocol https :inherit t :depth 1 :ref "021fc10df2e44faba4728d849ee767cf890aa51a"))
 (epl :source "lockfile" :date
      (25687 51434 18735 478000)
      :recipe
      (:package "epl" :repo "cask/epl" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "78ab7a85c08222cd15582a298a364774e3282ce6"))
 (posframe :source "lockfile" :date
           (25687 51434 7494 922000)
           :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "3b97dc180b03498103cfcc7f44e64150df440bf0"))
 (shrink-path :source "lockfile" :date
              (25687 51433 999514 702000)
              :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (git-commit :source "lockfile" :date
             (25687 51433 990375 604000)
             :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
                       ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
                       :old-names
                       (git-commit-mode)
                       :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (with-editor :source "lockfile" :date
   (25687 51433 981619 377000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref "df74385b455cd7687232ad189acfea16cb44dd04"))
 (async :source "lockfile" :date
        (25687 51433 972284 863000)
        :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "34feabe1142863a2c96f75afda1a2ae4aa0813f6"))
 (pcre2el :source "lockfile" :date
          (25687 51433 962937 9000)
          :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "b941ed8a96299868171fac625ecffec77de3e986"))
 (popup :source "lockfile" :date
        (25687 51433 953000 42000)
        :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "71cede0588c10526f88dd0375bce776ec2eed05e"))
 (rust-mode :source "lockfile" :date
            (25687 51433 942937 152000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e443ccf2884028d3b6cc550ff20e7c92dadccb68"))
 (xterm-color :source "lockfile" :date
              (25687 51433 932086 520000)
              :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "2ad407c651e90fff2ea85d17bf074cee2c022912"))
 (esxml :source "lockfile" :date
        (25687 51433 922692 292000)
        :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el")
                  :protocol https :inherit t :depth 1 :ref "225693096a587492d76bf696d1f0c25c61f7d531"))
 (kv :source "lockfile" :date
     (25687 51433 912214 753000)
     :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "721148475bce38a70e0b678ba8aa923652e8900e"))
 (yaml-mode :source "lockfile" :date
            (25687 51433 902066 935000)
            :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "b153150e0e77b4ec462d741cdb16956c6ae270d6")))

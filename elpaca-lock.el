((elpaca :source "lockfile" :date
         (25684 37469 768995 286000)
         :recipe
         (:protocol https :inherit t :depth 1 :repo "https://github.com/progfolio/elpaca.git" :ref "ab5f7156a681b7205b7235861ab2cdc730550382" :files
                    (:defaults
                     (:exclude "extensions"))
                    :build
                    (:not elpaca--activate-package)
                    :package "elpaca"))
 (elpaca-use-package :source "lockfile" :date
                     (25684 37469 760605 512000)
                     :recipe
                     (:package "elpaca-use-package" :repo "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el")
                               :main "extensions/elpaca-use-package.el" :build
                               (:not elpaca--compile-info)
                               :protocol https :inherit t :depth 1 :ref "ab5f7156a681b7205b7235861ab2cdc730550382"))
 (no-littering :source "lockfile" :date
               (25684 37469 751666 924000)
               :recipe
               (:package "no-littering" :fetcher github :repo "emacscollective/no-littering" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "6a8d50f067cc28cbeafdaa09c16644cb63a97cc0"))
 (compat :source "lockfile" :date
         (25684 37469 743149 619000)
         :recipe
         (:package "compat" :host github :files
                   ("*"
                    (:exclude ".git"))
                   :repo "emacs-straight/compat" :protocol https :inherit t :depth 1 :ref "cd28402790821ce95ea4c3ea0de5c2d9dcfce31f"))
 (gcmh :source "lockfile" :date
       (25684 37469 734291 907000)
       :recipe
       (:package "gcmh" :repo "koral/gcmh" :fetcher gitlab :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9"))
 (direnv :source "lockfile" :date
         (25684 37469 718819 511000)
         :recipe
         (:package "direnv" :fetcher github :repo "wbolster/emacs-direnv" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "268536f564b7eba99264a89a9149268eb4bc67ac"))
 (ws-butler :source "lockfile" :date
            (25684 37469 709460 687000)
            :recipe
            (:package "ws-butler" :fetcher github :repo "lewang/ws-butler" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e3a38d93e01014cd47bf5af4924459bd145fd7c4"))
 (command-log-mode :source "lockfile" :date
                   (25684 37469 699943 741000)
                   :recipe
                   (:package "command-log-mode" :fetcher github :repo "lewang/command-log-mode" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :protocol https :inherit t :depth 1 :ref "af600e6b4129c8115f464af576505ea8e789db27"))
 (project :source "lockfile" :date
          (25684 37469 693285 965000)
          :recipe
          (:package "project" :host github :files
                    ("*"
                     (:exclude ".git"))
                    :repo "emacs-straight/project" :protocol https :inherit t :depth 1 :ref "33511939473551b5cfa42de9a12d606b3d60a2cf"))
 (switch-window :source "lockfile" :date
                (25684 37469 684046 220000)
                :recipe
                (:package "switch-window" :repo "dimitri/switch-window" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "71ef2f54c97f3fd2e7ff7964d82e6562eb6282f7"))
 (rotate :source "lockfile" :date
         (25684 37469 678011 360000)
         :recipe
         (:package "rotate" :fetcher github :repo "daichirata/emacs-rotate" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "4e9ac3ff800880bd9b705794ef0f7c99d72900a6"))
 (avy :source "lockfile" :date
      (25684 37469 671137 844000)
      :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "be612110cb116a38b8603df367942e2bb3d9bdbe"))
 (default-text-scale :source "lockfile" :date
                     (25684 37469 663037 563000)
                     :recipe
                     (:package "default-text-scale" :fetcher github :repo "purcell/default-text-scale" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :protocol https :inherit t :depth 1 :ref "bfc0987c37e93742255d3b23d86c17096fda8e7e"))
 (doom-modeline :source "lockfile" :date
                (25684 37469 656054 186000)
                :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "8d9815186009956cb04c045739d31625d8dac6d5"))
 (beacon :source "lockfile" :date
         (25684 37469 647131 312000)
         :recipe
         (:package "beacon" :fetcher github :repo "Malabarba/beacon" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "85261a928ae0ec3b41e639f05291ffd6bf7c231c"))
 (general :source "lockfile" :date
          (25684 37469 638234 279000)
          :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "7ce8db297e3de258ec43802269438ac7f1918707"))
 (transient :source "lockfile" :date
            (25684 37469 630964 901000)
            :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "af7fe42bd46e24ca7852e73bd1691015c5bd2151"))
 (restart-emacs :source "lockfile" :date
                (25684 37469 623050 608000)
                :recipe
                (:package "restart-emacs" :fetcher github :repo "iqbalansari/restart-emacs" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "1607da2bc657fe05ae01f7fdf26f716eafead02c"))
 (snow :source "lockfile" :date
       (25684 37469 616028 678000)
       :recipe
       (:package "snow" :fetcher github :repo "alphapapa/snow.el" :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :host github :ref "be17977677fa29709a726715a1a1cba1bd299f68"))
 (keyfreq :source "lockfile" :date
          (25684 37469 608135 407000)
          :recipe
          (:package "keyfreq" :fetcher github :repo "dacap/keyfreq" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "dd88193cd7a91a92113121191573758ea2a3ceb1"))
 (vlf :source "lockfile" :date
      (25684 37469 601087 566000)
      :recipe
      (:package "vlf" :repo "m00natic/vlfi" :fetcher github :old-names
                (vlfi)
                :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c"))
 (sudo-edit :source "lockfile" :date
            (25684 37469 592027 314000)
            :recipe
            (:package "sudo-edit" :repo "nflath/sudo-edit" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "74eb1e6986461baed9a9269566ff838530b4379b"))
 (swiper :source "lockfile" :date
         (25684 37469 584128 106000)
         :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files
                   ("swiper.el")
                   :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (amx :source "lockfile" :date
      (25684 37469 579063 833000)
      :recipe
      (:package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "5b3aa1aae84f4a225cb8d26ab79a32f97693f023"))
 (ivy :source "lockfile" :date
      (25684 37469 570513 633000)
      :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el"))
                :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (ivy-rich :source "lockfile" :date
           (25684 37469 561634 619000)
           :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (all-the-icons-ivy-rich :source "lockfile" :date
                         (25684 37469 552045 388000)
                         :recipe
                         (:package "all-the-icons-ivy-rich" :fetcher github :repo "seagle0128/all-the-icons-ivy-rich" :files
                                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                   :protocol https :inherit t :depth 1 :ref "c098cc85123a401b0ab8f2afd3a25853e61d7d28"))
 (helpful :source "lockfile" :date
          (25684 37469 543147 657000)
          :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "8893e4ba49e4447b41ee08d93e58c23e07bc7514"))
 (counsel :source "lockfile" :date
          (25684 37469 534771 292000)
          :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files
                    ("counsel.el")
                    :protocol https :inherit t :depth 1 :ref "d28225e86f8dfb3825809ad287f759f95ee9e479"))
 (projectile :source "lockfile" :date
             (25684 37469 527642 645000)
             :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "271007c6611fcb08ddd326d7de9727c2ad5ef265"))
 (projectile-ripgrep :source "lockfile" :date
                     (25684 37469 519470 287000)
                     :recipe
                     (:package "projectile-ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
                               ("projectile-ripgrep.el")
                               :protocol https :inherit t :depth 1 :ref "b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a"))
 (counsel-projectile :source "lockfile" :date
                     (25684 37469 511973 855000)
                     :recipe
                     (:package "counsel-projectile" :fetcher github :repo "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :protocol https :inherit t :depth 1 :ref "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (nix-mode :source "lockfile" :date
           (25684 37469 502981 0)
           :recipe
           (:package "nix-mode" :fetcher github :repo "NixOS/nix-mode" :files
                     (:defaults
                      (:exclude "nix-company.el" "nix-mode-mmm.el"))
                     :protocol https :inherit t :depth 1 :ref "719feb7868fb567ecfe5578f6119892c771ac5e5"))
 (list-environment :source "lockfile" :date
                   (25684 37469 495740 187000)
                   :recipe
                   (:package "list-environment" :fetcher github :repo "dgtized/list-environment.el" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :protocol https :inherit t :depth 1 :ref "0a72a5a9c1abc090b25202a0387e3f766994b053"))
 (lsp-mode :source "lockfile" :date
           (25684 37469 488037 583000)
           :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.el")
                     :protocol https :inherit t :depth 1 :ref "cf30718ed5128753565452f476385dac1f7821d6"))
 (lsp-ui :source "lockfile" :date
         (25684 37469 480824 78000)
         :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources")
                   :protocol https :inherit t :depth 1 :ref "295d8984da06a745b0a36c56e28ce915bc389adb"))
 (company :source "lockfile" :date
          (25684 37469 473805 12000)
          :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons"
                               ("images/small" "doc/images/small/*.png"))
                    :protocol https :inherit t :depth 1 :ref "8a78f320019574bc35b5727f95b052b27918da20"))
 (orderless :source "lockfile" :date
            (25684 37469 466685 864000)
            :recipe
            (:package "orderless" :repo "oantolin/orderless" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
 (yasnippet :source "lockfile" :date
            (25684 37469 459940 157000)
            :recipe
            (:package "yasnippet" :repo "joaotavora/yasnippet" :fetcher github :files
                      ("yasnippet.el" "snippets")
                      :protocol https :inherit t :depth 1 :ref "5cbdbf0d2015540c59ed8ee0fcf4788effdf75b6"))
 (flycheck :source "lockfile" :date
           (25684 37469 452428 779000)
           :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "5f2ef177cb21ae8b73714575802beef04abd0f5e"))
 (exec-path-from-shell :source "lockfile" :date
                       (25684 37469 443639 302000)
                       :recipe
                       (:package "exec-path-from-shell" :fetcher github :repo "purcell/exec-path-from-shell" :files
                                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                 :protocol https :inherit t :depth 1 :ref "ddd24dc823de9a94435b4d8ea7569161657f31e2"))
 (auto-compile :source "lockfile" :date
               (25684 37469 435706 989000)
               :recipe
               (:package "auto-compile" :repo "emacscollective/auto-compile" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "cdb60240000aff141fbe26f1487fbfe499ff64dc"))
 (dap-mode :source "lockfile" :date
           (25684 37469 427088 65000)
           :recipe
           (:package "dap-mode" :repo "emacs-lsp/dap-mode" :fetcher github :files
                     (:defaults "icons")
                     :protocol https :inherit t :depth 1 :ref "24892f9e30aa9645d51b0a15dfebd75861e6c97f"))
 (all-the-icons :source "lockfile" :date
                (25684 37469 418584 310000)
                :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github :files
                          (:defaults "data")
                          :protocol https :inherit t :depth 1 :ref "8a7e70da843d99a36ad3c2b9e19924dcf533e5a0"))
 (all-the-icons-dired :source "lockfile" :date
                      (25684 37469 410798 874000)
                      :recipe
                      (:package "all-the-icons-dired" :repo "wyuenho/all-the-icons-dired" :fetcher github :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :protocol https :inherit t :depth 1 :ref "4564bec6bd3fd02dd870e6d2cfed37fe38bbc93a"))
 (doom-themes :source "lockfile" :date
              (25684 37469 402190 985000)
              :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el")
                        :protocol https :inherit t :depth 1 :ref "e4f0b006a516a35f53df2dce2ec116876c5cd7f9"))
 (dashboard :source "lockfile" :date
            (25684 37469 393992 716000)
            :recipe
            (:package "dashboard" :fetcher github :repo "emacs-dashboard/emacs-dashboard" :files
                      (:defaults "banners")
                      :protocol https :inherit t :depth 1 :ref "3fce60c285ed4d22a00d6f5b49335d038aa9cd41"))
 (magit :source "lockfile" :date
        (25684 37469 387614 445000)
        :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-rebase.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "Documentation/magit.texi" "Documentation/AUTHORS.md"
                   (:exclude "lisp/magit-libgit.el" "lisp/magit-libgit-pkg.el" "lisp/magit-section.el" "lisp/magit-section-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (magit-todos :source "lockfile" :date
              (25684 37469 380454 439000)
              :recipe
              (:package "magit-todos" :fetcher github :repo "alphapapa/magit-todos" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "7724259a008144b8cfc6cacdae3e764f207a03e7"))
 (git-messenger :source "lockfile" :date
                (25684 37469 372384 469000)
                :recipe
                (:package "git-messenger" :repo "emacsorphanage/git-messenger" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "eade986ef529aa2dac6944ad61b18de55cee0b76"))
 (git-gutter :source "lockfile" :date
             (25684 37469 364340 61000)
             :recipe
             (:package "git-gutter" :repo "emacsorphanage/git-gutter" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "1451e3149865b88de2402ce4296ee5608fadc5b2"))
 (rustic :source "lockfile" :date
         (25684 37469 357169 648000)
         :recipe
         (:package "rustic" :repo "brotzeit/rustic" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "39423d1cf4fa054c36bf9577356451f4c06ee148"))
 (pos-tip :source "lockfile" :date
          (25684 37469 349568 733000)
          :recipe
          (:package "pos-tip" :repo "pitkali/pos-tip" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "bfe74204d1201a33ace81898e7c485382817510a"))
 (clippy :source "lockfile" :date
         (25684 37469 341701 653000)
         :recipe
         (:package "clippy" :fetcher github :repo "Fuco1/clippy.el" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                   :protocol https :inherit t :depth 1 :ref "85aec3129ff17f71ea4541cfadbb7b56b31a7474"))
 (nov :source "lockfile" :date
      (25684 37469 333604 235000)
      :recipe
      (:package "nov" :fetcher git :url "https://depp.brause.cc/nov.el.git" :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "58c35e677e11f5c04a702b42ac753c80c8955089"))
 (k8s-mode :source "lockfile" :date
           (25684 37469 324412 541000)
           :recipe
           (:package "k8s-mode" :fetcher github :repo "TxGVNN/emacs-k8s-mode" :files
                     ("*.el"
                      ("snippets/k8s-mode" "snippets/k8s-mode/*"))
                     :protocol https :inherit t :depth 1 :ref "83266cecd6a39cdf57d124270646047860bfb7ab"))
 (hl-todo :source "lockfile" :date
          (25684 37469 315210 860000)
          :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "b27cddf7373408681cc949c8ef829f87a01ed3f3"))
 (org-make-toc :source "lockfile" :date
               (25684 37469 307234 268000)
               :recipe
               (:package "org-make-toc" :fetcher github :repo "alphapapa/org-make-toc" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                         :protocol https :inherit t :depth 1 :ref "26fbd6a7e1e7f8e473fe3a5f74faec715c3a05aa"))
 (org :source "lockfile" :date
      (25684 37469 300044 719000)
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
 (package-build :source "lockfile" :date
                (25684 37469 235137 915000)
                :recipe
                (:package "package-build" :repo "melpa/package-build" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "f23fd59f426f330d3d3ffb7077c97cea22469b70"))
 (champagne :source "lockfile" :date
            (25684 37469 203041 717000)
            :recipe
            (:protocol https :inherit t :depth 1 :type git :host github :repo "positron-solutions/champagne" :package "champagne" :ref "af8585907b6085170fad5adc2fbe78ef299e3a42"))
 (parrot :source "lockfile" :date
         (25684 37469 195285 195000)
         :recipe
         (:package "parrot" :fetcher github :repo "positron-solutions/parrot" :files
                   (:defaults "img")
                   :protocol https :inherit t :depth 1 :host github :ref "49d3493e1041bb564077d8b83ad158d16fbbe51d"))
 (dash :source "lockfile" :date
       (25684 37469 188898 55000)
       :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files
                 ("dash.el" "dash.texi")
                 :protocol https :inherit t :depth 1 :ref "96eaba028ac069ea0e5cc70de15b0229126a054a"))
 (xref :source "lockfile" :date
       (25684 37469 179366 792000)
       :recipe
       (:package "xref" :host github :files
                 ("*"
                  (:exclude ".git"))
                 :repo "emacs-straight/xref" :protocol https :inherit t :depth 1 :ref "420511e20187d0c6c8680c0e63ae8810f84dee00"))
 (shrink-path :source "lockfile" :date
              (25684 37469 171604 682000)
              :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (s :source "lockfile" :date
    (25684 37469 162486 113000)
    :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (f :source "lockfile" :date
    (25684 37469 154654 92000)
    :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
              :protocol https :inherit t :depth 1 :ref "af7d37c619010b576fd22b50c62c71ff33093f3c"))
 (elisp-refs :source "lockfile" :date
             (25684 37469 145618 215000)
             :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults
                        (:exclude "elisp-refs-bench.el"))
                       :protocol https :inherit t :depth 1 :ref "bf3cca8f74065b1b31036f461e3a093b162311bd"))
 (ripgrep :source "lockfile" :date
          (25684 37469 138879 842000)
          :recipe
          (:package "ripgrep" :repo "nlamirault/ripgrep.el" :fetcher github :files
                    ("ripgrep.el")
                    :protocol https :inherit t :depth 1 :ref "b6bd5beb0c11348f1afd9486cbb451d0d2e3c45a"))
 (magit-section :source "lockfile" :date
                (25684 37469 133365 21000)
                :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "lisp/magit-section-pkg.el" "docs/magit-section.texi" "Documentation/magit-section.texi")
                          :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (ht :source "lockfile" :date
     (25684 37469 127425 634000)
     :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "3c1677f1bf2ded2ab07edffb7d17def5d2b5b6f6"))
 (spinner :source "lockfile" :date
          (25684 37469 119988 427000)
          :recipe
          (:package "spinner" :host github :files
                    ("*"
                     (:exclude ".git"))
                    :repo "emacs-straight/spinner" :protocol https :inherit t :depth 1 :ref "634529bb3173e09b37499f636de70abf29d9fa8a"))
 (markdown-mode :source "lockfile" :date
                (25684 37469 111842 260000)
                :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                          :protocol https :inherit t :depth 1 :ref "5d98592fe516748034d8baf92d7c0ba045e1f87a"))
 (lv :source "lockfile" :date
     (25684 37469 103802 182000)
     :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files
               ("lv.el")
               :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (eldoc :source "lockfile" :date
        (25684 37469 96027 13000)
        :recipe
        (:package "eldoc" :host github :files
                  ("*"
                   (:exclude ".git"))
                  :repo "emacs-straight/eldoc" :protocol https :inherit t :depth 1 :ref "bf2e88dcf00e6554e24b517aa315527011042fae"))
 (pkg-info :source "lockfile" :date
           (25684 37469 88967 229000)
           :recipe
           (:package "pkg-info" :repo "emacsorphanage/pkg-info" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "76ba7415480687d05a4353b27fea2ae02b8d9d61"))
 (let-alist :source "lockfile" :date
            (25684 37469 80396 635000)
            :recipe
            (:package "let-alist" :host github :files
                      ("*"
                       (:exclude ".git"))
                      :repo "emacs-straight/let-alist" :protocol https :inherit t :depth 1 :ref "021fc10df2e44faba4728d849ee767cf890aa51a"))
 (epl :source "lockfile" :date
      (25684 37469 73735 996000)
      :recipe
      (:package "epl" :repo "cask/epl" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                :protocol https :inherit t :depth 1 :ref "78ab7a85c08222cd15582a298a364774e3282ce6"))
 (lsp-treemacs :source "lockfile" :date
               (25684 37469 59526 543000)
               :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher github :files
                         (:defaults "icons")
                         :protocol https :inherit t :depth 1 :ref "eeb96b05e677147cf40292b86a1e5e8f73a8a586"))
 (posframe :source "lockfile" :date
           (25684 37469 50920 609000)
           :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                     :protocol https :inherit t :depth 1 :ref "3b97dc180b03498103cfcc7f44e64150df440bf0"))
 (lsp-docker :source "lockfile" :date
             (25684 37469 44067 626000)
             :recipe
             (:package "lsp-docker" :repo "emacs-lsp/lsp-docker" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "124a2f0d623b29734dc172db8c4e7680522ced3a"))
 (treemacs :source "lockfile" :date
           (25684 37469 37208 917000)
           :recipe
           (:package "treemacs" :fetcher github :repo "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el" "src/scripts/treemacs*.py"
                                (:exclude "src/extra/*"))
                     :protocol https :inherit t :depth 1 :ref "983ea5a66801a5c1f6e32e3d515bd48761677ac6"))
 (ace-window :source "lockfile" :date
             (25684 37469 27869 927000)
             :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                       :protocol https :inherit t :depth 1 :ref "77115afc1b0b9f633084cf7479c767988106c196"))
 (pfuture :source "lockfile" :date
          (25684 37469 20712 86000)
          :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (hydra :source "lockfile" :date
        (25684 37469 14201 47000)
        :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults
                   (:exclude "lv.el"))
                  :protocol https :inherit t :depth 1 :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (cfrs :source "lockfile" :date
       (25684 37469 6488 456000)
       :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (yaml :source "lockfile" :date
       (25684 37468 997890 205000)
       :recipe
       (:package "yaml" :repo "zkry/yaml.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "a19fbf948a945571300e5a20ff1dbfa6ecfa0d16"))
 (git-commit :source "lockfile" :date
             (25684 37468 990444 827000)
             :recipe
             (:package "git-commit" :fetcher github :repo "magit/magit" :files
                       ("lisp/git-commit.el" "lisp/git-commit-pkg.el")
                       :old-names
                       (git-commit-mode)
                       :protocol https :inherit t :depth 1 :ref "6067f92c0195616707b25e23c2d4c0dd81928fd8"))
 (with-editor :source "lockfile" :date
   (25684 37468 980228 908000)
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
             :protocol https :inherit t :depth 1 :ref "df74385b455cd7687232ad189acfea16cb44dd04"))
 (async :source "lockfile" :date
        (25684 37468 972122 690000)
        :recipe
        (:package "async" :repo "jwiegley/emacs-async" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "34feabe1142863a2c96f75afda1a2ae4aa0813f6"))
 (pcre2el :source "lockfile" :date
          (25684 37468 962031 857000)
          :recipe
          (:package "pcre2el" :fetcher github :repo "joddie/pcre2el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                    :protocol https :inherit t :depth 1 :ref "b941ed8a96299868171fac625ecffec77de3e986"))
 (popup :source "lockfile" :date
        (25684 37468 953566 305000)
        :recipe
        (:package "popup" :fetcher github :repo "auto-complete/popup-el" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "71cede0588c10526f88dd0375bce776ec2eed05e"))
 (rust-mode :source "lockfile" :date
            (25684 37468 945878 158000)
            :recipe
            (:package "rust-mode" :repo "rust-lang/rust-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "e443ccf2884028d3b6cc550ff20e7c92dadccb68"))
 (xterm-color :source "lockfile" :date
              (25684 37468 939328 427000)
              :recipe
              (:package "xterm-color" :repo "atomontage/xterm-color" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                        :protocol https :inherit t :depth 1 :ref "2ad407c651e90fff2ea85d17bf074cee2c022912"))
 (esxml :source "lockfile" :date
        (25684 37468 931500 737000)
        :recipe
        (:package "esxml" :fetcher github :repo "tali713/esxml" :files
                  ("esxml.el" "esxml-query.el")
                  :protocol https :inherit t :depth 1 :ref "225693096a587492d76bf696d1f0c25c61f7d531"))
 (kv :source "lockfile" :date
     (25684 37468 923664 386000)
     :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "721148475bce38a70e0b678ba8aa923652e8900e"))
 (yaml-mode :source "lockfile" :date
            (25684 37468 915863 96000)
            :recipe
            (:package "yaml-mode" :repo "yoshiki/yaml-mode" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                      :protocol https :inherit t :depth 1 :ref "b153150e0e77b4ec462d741cdb16956c6ae270d6"))
 (emacsql :source "lockfile" :date
          (25684 37468 908044 206000)
          :recipe
          (:package "emacsql" :fetcher github :repo "magit/emacsql" :files
                    (:defaults "sqlite")
                    :protocol https :inherit t :depth 1 :ref "64012261f65fcdd7ea137d1973ef051af1dced42"))
 (alert :source "lockfile" :date
        (25684 37468 902153 987000)
        :recipe
        (:package "alert" :fetcher github :repo "jwiegley/alert" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "c762380ff71c429faf47552a83605b2578656380"))
 (gntp :source "lockfile" :date
       (25684 37468 893321 209000)
       :recipe
       (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                 :protocol https :inherit t :depth 1 :ref "767571135e2c0985944017dc59b0be79af222ef5"))
 (log4e :source "lockfile" :date
        (25684 37468 886015 25000)
        :recipe
        (:package "log4e" :repo "aki2o/log4e" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                  :protocol https :inherit t :depth 1 :ref "737d275eac28dbdfb0b26d28e99da148bfce9d16"))
 (xr :source "lockfile" :date
     (25684 37468 877012 112000)
     :recipe
     (:package "xr" :host github :files
               ("*"
                (:exclude ".git"))
               :repo "emacs-straight/xr" :protocol https :inherit t :depth 1 :ref "dad60e70a58b54fc3a8dca4bc5f8f9b98600f4c2"))
 (ov :source "lockfile" :date
     (25684 37468 857641 97000)
     :recipe
     (:package "ov" :fetcher github :repo "emacsorphanage/ov" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "c5b9aa4e1b00d702eb2caedd61c69a22a5fa1fab"))
 (peg :source "lockfile" :date
      (25684 37468 849325 774000)
      :recipe
      (:package "peg" :host github :files
                ("*"
                 (:exclude ".git"))
                :repo "emacs-straight/peg" :protocol https :inherit t :depth 1 :ref "5d4ed356ca89acdf52a3e7e7f8e2408b808552c4"))
 (ts :source "lockfile" :date
     (25684 37468 841719 621000)
     :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
               :protocol https :inherit t :depth 1 :ref "552936017cfdec89f7fc20c254ae6b37c3f22c5b")))

{ lib-and-package }:
{ config, pkgs, lib, ... }:

{
  config = {
    home.packages = [
      lib-and-package
    ];

    # link emacs-vterm module and vterm.el file into convenient load path
    home.file.".emacs.d/vendor/emacs-vterm".source = lib-and-package;
    # link loading shim into place and include in posimacs-init.el shim

    # can be modified for fish etc, which vterm supports
    programs.bash.initExtra = ''
      # Extra bash shell configuration for vterm in emacs

      vterm_printf() {
          if [ -n "$TMUX" ] \
              && { [ "''${TERM%%-*}" = "tmux" ] \
                  || [ "''${TERM%%-*}" = "screen" ]; }; then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }

      if [ "$INSIDE_EMACS" = 'vterm' ]; then
          clear() {
              vterm_printf "51;Evterm-clear-scrollback";
              tput clear;
          }
      fi

      # End vterm configuration
    '';
  };
}

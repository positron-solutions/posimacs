{ emacs-vterm }: { config, pkgs, lib, specialArgs, ... }:

let
  cfg = config.posimacs;
in {
  config = {
    home.packages = [
      emacs-vterm
    ];

    # link emacs-vterm module into convenient load path
    home.file.".emacs.d/vendor/emacs-vterm".source = emacs-vterm;
    # link loading shim into place and include in posimacs-init.el shim

    # TODO detect default shell and support zsh.initExtra as well
    programs.bash.initExtra = ''
      # Extra bash shell configuration for vterm in emacs

      vterm_printf(){
          if [ -n "$TMUX" ]; then
              # Tell tmux to pass the escape sequences through
              # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "''${TERM%%-*}" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }

      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
        function clear(){
          vterm_printf "51;Evterm-clear-scrollback";
          tput clear;
        }
      fi

      PROMPT_COMMAND='echo -ne "\033]0;''${HOSTNAME}:''${PWD}\007"'

      vterm_prompt_end(){
          vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
      }
      PS1=$PS1'\[$(vterm_prompt_end)\]'

      vterm_cmd() {
          local vterm_elisp
          vterm_elisp=""
          while [ $# -gt 0 ]; do
              vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | ${pkgs.gnused}/bin/sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
              shift
          done
          vterm_printf "51;E$vterm_elisp"
      }

      # End vterm configuration
    '';
  };
}

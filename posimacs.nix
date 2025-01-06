{ emacs-jinx, emacs-overlay, emacs29-src }:
{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs;
  client-or-server = "emacsclient --create-frame --alternate-editor emacs";
  # terminal version of the above
  # client-or-server = "emacsclient -nw --create-frame -a emacs";
in
{
  options.posimacs = {
    aliases = lib.mkOption {
      default = pkgs.stdenv.isLinux;
      example = true;
      description = "Succinct shortcuts";
      type = lib.types.bool;
    };
  };

  # import modules within modules for composition / dependency
  imports = [
    emacs-jinx
  ];

  config = {
    # customizing the build or version of an Emacs packagep
    nixpkgs.overlays = [
      emacs-overlay.overlays.default
    ];

    # Install packages from the top level package set if your module depends on them
    home.packages = with pkgs; [
      aspell
      aspellDicts.en
      nerdfonts
      nil
      ripgrep # projectile-ripgrep function relies on this
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      roboto
      roboto-mono
      roboto-slab
      symbola # Emacs can use this font to override symbols
    ];

    programs.emacs = {
      enable = true;
      package = pkgs.emacs-unstable;
      extraPackages = (epkgs: [epkgs.treesit-grammars.with-all-grammars]);
    };

    # Fontconfig will ensure that fonts installed in home.packages
    # are available
    fonts.fontconfig.enable = true;

    programs.bash.shellAliases = lib.mkIf cfg.aliases {
      # How to break a nano habit
      # "nano" = client-or-server;
      "emacs" = client-or-server;
      "vi" = client-or-server;
      "vim" = client-or-server;
    };

    home.sessionVariables = lib.mkIf cfg.aliases ({
      # Use standalone GUI emacs and fall back to terminal if GUI cannot load
      EDITOR = client-or-server;
      ALTERNATE_EDITOR = "TERM=xterm-256color emacs -nw";
    });

    # some more Korean fonts for extra nice looking Org mode
    home.file.".local/share/fonts/GowunBatang-Bold.ttf".source = ./fonts/GowunBatang-Bold.ttf;
    home.file.".local/share/fonts/GowunBatang-Regular.ttf".source = ./fonts/GowunBatang-Regular.ttf;
    home.file.".local/share/fonts/GowunDodum-Regular.ttf".source = ./fonts/GowunDodum-Regular.ttf;
  };
}

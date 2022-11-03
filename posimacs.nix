{ emacs-vterm, rust, emacs-overlay }:
{ config, pkgs, lib, specialArgs, ... }:

let
  posimacs = specialArgs.inputs.posimacs;
  cfg = config.posimacs;
in {
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
    emacs-vterm
    rust
  ];

  config = {
    nixpkgs.overlays = [ emacs-overlay.overlays.default ];

    # Install packages from the top level package set if your module depends on them
    home.packages = with pkgs; [
      ripgrep # projectile-ripgrep function relies on this
      roboto
      roboto-mono
      roboto-slab
      symbola # Emacs can use this font to override symbols
    ];

    programs.emacs = {
      package = pkgs.emacsNativeComp;
      enable = true;
    };

    # Fontconfig will ensure that fonts installed in home.packages
    # are available
    fonts.fontconfig.enable = true;

    programs.bash.shellAliases = lib.mkIf cfg.aliases (if config.services.emacs.enable then {
      "nano" = "emacsclient --create-frame";  # How to break a nano habit
      # "nano" = "emacsclient -nw --create-frame";  # Terminal version of above
      "emacs" = "emacsclient --create-frame"; # Don't re-use frames
      "vi" = "emacsclient --create-frame";
      "vim" = "emacsclient --create-frame";
    } else {
      "nano" = "emacs";
      "vi" = "emacs";
      "vim" = "emacs";
    });

    home.sessionVariables = lib.mkIf cfg.aliases (if config.services.emacs.enable then {
      # Use a new client window and fall back to terminal standalone if client fails
      EDITOR = "emacsclient --create-frame";
      ALTERNATE_EDITOR = "TERM=xterm-256color emacs -nw";
    } else {
      # Use standalone GUI emacs and fall back to terminal if GUI cannot load
      EDITOR = "emacs";
      ALTERNATE_EDITOR = "TERM=xterm-256color emacs -nw";
    });

    # pre-install fonts for all-the-icons
    home.file.".local/share/fonts/all-the-icons.ttf".source = ./fonts/all-the-icons.ttf;
    home.file.".local/share/fonts/file-icons.ttf".source = ./fonts/file-icons.ttf;
    home.file.".local/share/fonts/fontawesome.ttf".source = ./fonts/fontawesome.ttf;
    home.file.".local/share/fonts/material-design-icons.ttf".source = ./fonts/material-design-icons.ttf;
    home.file.".local/share/fonts/octicons.ttf".source = ./fonts/octicons.ttf;
    home.file.".local/share/fonts/weathericons.ttf".source = ./fonts/weathericons.ttf;
  };
}

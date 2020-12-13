{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs-options;
  sources = import nix/sources.nix;
  emacs-pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.emacs-overlay)
    ];
  };

in {
  options.posimacs-options = {
    aliases = lib.mkOption {
      default = pkgs.stdenv.isLinux;
      example = true;
      description = "Succinct shortcuts";
      type = lib.types.bool;
    };
  };

  # import modules within modules for composition / dependency
  imports = [
    ./rust-analyzer
    ./rust
  ];

  config = {
    # Install packages from the top level package set if your module depends on them
    home.packages = with pkgs; [
      niv
      nix-index
      nix-prefetch-git
      ripgrep # projectile-ripgrep function relies on this
      symbola
    ];

    programs.emacs = {
      package = emacs-pkgs.emacsUnstable;
      enable = true;
    };

    # symbola is a good font for nice looking unicode symbols
    # that Emacs uses to override the font used to render code
    fonts.fontconfig.enable = true;

    programs.bash.shellAliases = lib.mkIf cfg.aliases (if config.services.emacs.enable then {
      "nano" = "emacsclient --create-frame";  # How to break a nano habit
      # "nano" = "emacsclient -nw --create-frame";  # Terminal version of above
      "emacs" = "emacsclient --create-frame"; # Don't re-use frames
    } else {
      "nano" = "emacs";
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

    # configs and theme
    home.file.".emacs.d/posimacs-fastload.el".source = ./posimacs-fastload.el;
    home.file.".emacs.d/posimacs-defaults.el".source = ./posimacs-defaults.el;
    home.file.".emacs.d/posimacs-minibuffer.el".source = ./posimacs-minibuffer.el;
    home.file.".emacs.d/posimacs-prog.el".source = ./posimacs-prog.el;
    home.file.".emacs.d/posimacs-vc.el".source = ./posimacs-vc.el;

    # pre-install fonts for all-the-icons
    home.file.".local/share/fonts/all-the-icons.ttf".source = ./fonts/all-the-icons.ttf;
    home.file.".local/share/fonts/file-icons.ttf".source = ./fonts/file-icons.ttf;
    home.file.".local/share/fonts/fontawesome.ttf".source = ./fonts/fontawesome.ttf;
    home.file.".local/share/fonts/material-design-icons.ttf".source = ./fonts/material-design-icons.ttf;
    home.file.".local/share/fonts/octicons.ttf".source = ./fonts/octicons.ttf;
    home.file.".local/share/fonts/weathericons.ttf".source = ./fonts/weathericons.ttf;
  };
}

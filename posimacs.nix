{ emacs-jinx, rust, emacs-overlay, emacs29-src }:
{ config, pkgs, lib, ... }:

let
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
    emacs-jinx
    rust
  ];

  config = {

    # customizing the build or version of an Emacs package
    nixpkgs.overlays = [ 
      emacs-overlay.overlays.default
      # (final : prev: {
      #   emacs = prev.emacs.overrideAttrs (old: {
      #     name = "emacs29";
      #     version = emacs29-src.shortRev;
      #     src = emacs29-src;
      #   });
      #  })
    ];

    # Install packages from the top level package set if your module depends on them
    home.packages = with pkgs; [
      ripgrep # projectile-ripgrep function relies on this
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      roboto
      roboto-mono
      roboto-slab
      symbola # Emacs can use this font to override symbols
    ];

    programs.emacs = {
      package = (pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages
        (epkgs: [epkgs.treesit-grammars.with-all-grammars]);
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

    # some more Korean fonts for extra nice looking Org mode
    home.file.".local/share/fonts/GowunBatang-Bold.ttf".source = ./fonts/GowunBatang-Bold.ttf;
    home.file.".local/share/fonts/GowunBatang-Regular.ttf".source = ./fonts/GowunBatang-Regular.ttf;
    home.file.".local/share/fonts/GowunDodum-Regular.ttf".source = ./fonts/GowunDodum-Regular.ttf;
  };
}

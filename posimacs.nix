{ emacs-jinx, rust, emacs-overlay, emacs29-src }:
{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs;
  client-or-server = "emacsclient --create-frame -a '/usr/bin/env emacs'";
  # terminal version of the above
  # client-or-server = "emacsclient -nw --create-frame -a '/usr/bin/env emacs'";
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
      # Add an explicit source to the flake if you require
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
      nixpkgs-fmt
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
      package = (pkgs.emacsPackagesFor pkgs.emacs29-gtk3).emacsWithPackages
        (epkgs: [epkgs.treesit-grammars.with-all-grammars]);
    };

    # Fontconfig will ensure that fonts installed in home.packages
    # are available
    fonts.fontconfig.enable = true;

    programs.bash.shellAliases = lib.mkIf cfg.aliases (if config.services.emacs.enable then {
      # How to break a nano habit
      # "nano" = client-or-server;
      "emacs" = client-or-server;
      "vi" = client-or-server;
      "vim" = client-or-server;
    } else {});

    home.sessionVariables = lib.mkIf cfg.aliases ({
      # Use standalone GUI emacs and fall back to terminal if GUI cannot load
      EDITOR = client-or-server;
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

{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs;
  sources = import nix/sources.nix;
  emacs-pkgs = import sources.nixpkgs {
    overlays = [
      (import sources.emacs-overlay)
    ];
  };
in {
  options.posimacs = {
    aliases = lib.mkOption {
      default = pkgs.stdenv.isLinux;
      example = true;
      description = "Succinct shortcuts";
      type = lib.types.bool;
    };

    modules = lib.mkOption {
      type = lib.types.listOf lib.types.string;
      default = [];
      description = "lisp files posimacs-init.el shim will load";
      example = ["posimacs-terminal.el"];
    };

    earlyModules = lib.mkOption {
      type = lib.types.listOf lib.types.string;
      default = [];
      description = "lisp files posimacs-early-init.el shim will load";
      example = ["posimacs-fastload.el"];
    };
  };

  # import modules within modules for composition / dependency
  imports = [ ];

  config = let

    posimacsEarlyFileList = if
      (builtins.length cfg.modules) > 0
      then lib.concatImapStrings
        (pos: file:
          let
            close = if (pos == builtins.length cfg.earlyModules) then "\"" else "\"\n";
          in
            if pos == 1
            then "\"" + file + close
            else "                        \"" + file + close)
        cfg.earlyModules
      else "";

    posimacsFileList = if
      (builtins.length cfg.modules) > 0
      then lib.concatImapStrings
        (pos: file:
          let
            close = if (pos == builtins.length cfg.modules) then "\"" else "\"\n";
          in
            if pos == 1
            then "\"" + file + close
            else "                        \"" + file + close)
        cfg.modules
      else "";
  in {
    # Install packages from the top level package set if your module depends on them
    home.packages = with pkgs; [
      ripgrep # projectile-ripgrep function relies on this
      symbola # Emacs can use this font to override symbols
    ];

    programs.emacs = {
      package = emacs-pkgs.emacsUnstable;
      enable = true;
    };

    # Fontconfig will ensure that fonts installed in home.packages
    # are available
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

    home.file.".emacs.d/posimacs-fastload.el".source = ./posimacs-fastload.el;

    home.file.".emacs.d/posimacs-init.el".text = ''
    ;;; posimacs-init.el --- Posimacs init

    ;;; Commentary:
    ;; This list was constructed via home manager and this shim provides a stable
    ;; target for init.el to load so that init.el doesn't need to change when modules
    ;; change.
    ;;
    ;; Simply load this file in your init.el to pick up on modules configured through
    ;; posimacs.
    ;;
    ;; (load (expand-file-name "posimacs-init.el" user-emacs-directory))

    ;;; Code:
    (let ((posimacs-files '(${posimacsFileList})))
      (dolist (file-name posimacs-files)
        (load (expand-file-name file-name user-emacs-directory))))

    ;;; posimacs-init.el ends here
    '';

    posimacs.modules = [
      "posimacs-defaults.el"
      "posimacs-minibuffer.el"
      "posimacs-prog.el"
      "posimacs-vc.el"
    ];

    home.file.".emacs.d/posimacs-early-init.el".text = ''
    ;;; posimacs-early-init.el --- Posimacs init

    ;;; Commentary:
    ;; This list was constructed via home manager and this shim provides a stable
    ;; target for early-init.el to load so that early-init.el doesn't need to change
    ;; when modules change.
    ;;
    ;; Simply load this file in your early-init.el to pick up on modules configured
    ;; through posimacs.
    ;;
    ;; (load (expand-file-name "posimacs-early-init.el" user-emacs-directory))

    ;;; Code:
    (let ((posimacs-files '(${posimacsEarlyFileList})))
      (dolist (file-name posimacs-files)
        (load (expand-file-name file-name user-emacs-directory))))

    ;;; posimacs-early-init.el ends here
    '';

    posimacs.earlyModules = [
      "posimacs-fastload.el"
    ];

    # pre-install fonts for all-the-icons
    home.file.".local/share/fonts/all-the-icons.ttf".source = ./fonts/all-the-icons.ttf;
    home.file.".local/share/fonts/file-icons.ttf".source = ./fonts/file-icons.ttf;
    home.file.".local/share/fonts/fontawesome.ttf".source = ./fonts/fontawesome.ttf;
    home.file.".local/share/fonts/material-design-icons.ttf".source = ./fonts/material-design-icons.ttf;
    home.file.".local/share/fonts/octicons.ttf".source = ./fonts/octicons.ttf;
    home.file.".local/share/fonts/weathericons.ttf".source = ./fonts/weathericons.ttf;

    # lisp files available to load via posimacsModules
    home.file.".emacs.d/posimacs-defaults.el".source = ./posimacs-defaults.el;
    home.file.".emacs.d/posimacs-minibuffer.el".source = ./posimacs-minibuffer.el;
    home.file.".emacs.d/posimacs-prog.el".source = ./posimacs-prog.el;
    home.file.".emacs.d/posimacs-vc.el".source = ./posimacs-vc.el;
  };
}

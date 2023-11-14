{ emacs-jinx }: { config, pkgs, lib, ... }:

let
  cfg = config.posimacs;
in {
  config = {
    home.packages = [
      emacs-jinx
    ];

    # link jinx module into convenient load path
    home.file.".emacs.d/vendor/emacs-jinx".source = emacs-jinx;
  };
}

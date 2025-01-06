{ emacs-jinx, korean-hunspell }: { config, pkgs, lib, ... }:

let
  cfg = config.posimacs;
in
{
  config = {
    home.packages = [
      emacs-jinx
      korean-hunspell
      pkgs.hunspell
      pkgs.hunspellDicts.en_US
    ];

    # link jinx module into convenient load path
    home.file.".emacs.d/vendor/emacs-jinx".source = emacs-jinx;
  };
}

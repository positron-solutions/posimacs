{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-21.11";
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm?rev=2681120b770573044832ba8c22ccbac192e1a294";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat, emacs-vterm-src}:
    flake-utils.lib.eachDefaultSystem (system:

      let
        pkgs = import nixpkgs { inherit system; };

        emacs-vterm = pkgs.stdenv.mkDerivation {
          name = "emacs-vterm";
          src = emacs-vterm-src;
          nativeBuildInputs = with pkgs; [
            cmake
            libtool
            glib.dev
          ];

          buildInputs = with pkgs; [
            glib.out
            libvterm-neovim
            ncurses
          ];

          cmakeFlags = [
            "-DUSE_SYSTEM_LIBVTERM=yes"
          ];

          preConfigure = ''
            echo "include_directories(\"${pkgs.glib.out}/lib/glib-2.0/include\")" >> CMakeLists.txt
            echo "include_directories(\"${pkgs.glib.dev}/include/glib-2.0\")" >> CMakeLists.txt
            echo "include_directories(\"${pkgs.ncurses.dev}/include\")" >> CMakeLists.txt
            echo "include_directories(\"${pkgs.libvterm-neovim}/include\")" >> CMakeLists.txt
          '';

          installPhase = ''
            mkdir -p $out
            cp ../vterm-module.so $out
            cp ../vterm.el $out
         '';
        };
      in rec {
        packages = {
          emacs-vterm = emacs-vterm;
        };

        defaultPackage = packages.emacs-vterm;
      });
}

{ pkgs, ...}:
let
  emacs-vterm-src = pkgs.fetchFromGitHub {
    owner =  "akermu";
    repo = "emacs-libvterm";
    rev = "a670b786539d3c8865d8f68fe0c67a2d4afbf1aa";
    sha256 = "0s244crjkbzl2jhp9m4sm1xdhbpxwph0m3jg18livirgajvdz6hn";
  };
in  pkgs.stdenv.mkDerivation {
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
}

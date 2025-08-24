{
  description = "Home manager module for Emacs vterm dependencies & integration.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/25.05";
    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils,  emacs-vterm-src}:
   flake-utils.lib.eachDefaultSystem (system:
     let
       pkgs = import nixpkgs { inherit system; };

       libvterm = (pkgs.libvterm-neovim.override {
         stdenv = pkgs.llvmPackages.stdenv;
       }).overrideAttrs(old: {

         # The nixpkgs derivation uses the provided Makefile, which depends on
         # libtool.  It's kind of overkill for this build and libtool seems to
         # prefer gcc tools.  So we just sidestepped it with a vibe coded manual
         # build script.
         buildPhase = ''
           mkdir -p build

           # Compile .c files to .o
           for src in src/*.c; do
             obj="build/$(basename $src .c).o"
             echo "CC $src -> $obj"
             $CC $NIX_CFLAGS_COMPILE -fPIC -Iinclude -std=c99 -Wall -Wpedantic \
               -c "$src" -o "$obj"
           done

           # Link shared library
           echo "LINK build/libvterm.so"
           $CC $NIX_CFLAGS_COMPILE $NIX_LDFLAGS -shared -fuse-ld=$LD build/*.o -o build/libvterm.so
         '';

         installPhase = ''
           mkdir -p $out/lib $out/include $out/bin
           cp build/libvterm.so $out/lib/
           cp include/*.h $out/include/
         '';

         NIX_CFLAGS_COMPILE = toString ([
           "-O2"
           "-march=znver2"
           "-mtune=znver2"
           "-flto=full"
           # "-fprofile-generate"
           # "-fprofile-use=$HOME/.cache/emacs/emacs.profdata"
         ]);
       });

       lib-and-package = pkgs.stdenv.mkDerivation {
         stdenv = pkgs.llvmPackages.stdenv;
         name = "emacs-vterm";
         src = emacs-vterm-src;
         nativeBuildInputs = with pkgs; [ cmake ];
         buildInputs = with pkgs; [ glib.out libvterm ];
         installPhase = ''
           runHook preInstall
           mkdir -p $out/lib
           cp ../vterm-module.so "''${out}/"
           # Package is added to load path in ./posimacs-terminal.el
           cp ../vterm.el "''${out}/"
           runHook postInstall
         '';

        preConfigure = ''
          export CC=${pkgs.llvmPackages.clang}/bin/clang
          export CXX=${pkgs.llvmPackages.clang}/bin/clang++
          export AR=${pkgs.llvm}/bin/llvm-ar
          export NM=${pkgs.llvm}/bin/llvm-nm
          export LD=${pkgs.lld}/bin/ld.lld
          export RANLIB=${pkgs.llvm}/bin/llvm-ranlib
        '';

        NIX_CFLAGS_COMPILE = toString ([
          "-O2"
          "-march=znver2"
          "-mtune=znver2"
          "-flto=full"
          "-fprofile-generate"
          # "-fprofile-use=$HOME/.cache/emacs/emacs.profdata"
        ]);
       };

       # Create the module and inject its dependency we declared above
       module = import ./vterm.nix { inherit lib-and-package; };

     in rec {
       homeConfigurations = {
         emacs-vterm = module;
         default = homeConfigurations.emacs-vterm;
       };
       # For debugging the build only
       packages = rec {
         emacs-vterm = lib-and-package;
         default = packages.emacs-vterm;
       };
     });
}

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

       lib-and-package = pkgs.stdenv.mkDerivation {
         name = "emacs-vterm";
         src = emacs-vterm-src;
         nativeBuildInputs = with pkgs; [ cmake ];
         buildInputs = with pkgs; [ glib.out libvterm-neovim ];
         installPhase = ''
           runHook preInstall
           mkdir -p $out/lib
           cp ../vterm-module.so "''${out}/"
           # Package is added to load path in ./posimacs-terminal.el
           cp ../vterm.el "''${out}/"
           runHook postInstall
         '';
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

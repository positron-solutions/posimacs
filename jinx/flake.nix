{
  description = "Home manager module for Emacs vterm dependencies & integration.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-23.05";
    jinx-src = {
      url = "github:minad/jinx?ref=0.8";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, jinx-src}:
    flake-utils.lib.eachDefaultSystem (system:

      let
        pkgs = import nixpkgs { inherit system; };

        emacs-jinx = pkgs.stdenv.mkDerivation {
          name = "emacs-jinx";
          src = jinx-src;
          buildInputs = with pkgs; [ enchant.out ];
          buildPhase = ''
            runHook preBuild
            gcc \
              -Wl,-Bdynamic ${pkgs.enchant}/lib/libenchant-2.so \
              -I${pkgs.enchant.dev}/include/enchant-2/ \
              -I. \
              -shared -O2 jinx-mod.c -o jinx-mod.so
            runHook postBuild
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp jinx-mod.so "''${out}/"
            # copy the module too for version synchronization
            cp jinx.el "''${out}/"
            runHook postInstall
          '';
        };

        # Create the module and inject its dependency we declared above
        module = import ./jinx.nix { inherit emacs-jinx; };

      in rec {
        homeConfigurations = {
          emacs-jinx = module;
          default = homeConfigurations.emacs-jinx;
        };
        packages = rec {
          inherit emacs-jinx;
          default = packages.emacs-jinx;
        };
      });
}

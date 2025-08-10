{
  description = "Home manager module for Emacs dependencies & integration.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-24.05";
    jinx-src = {
      url = "github:minad/jinx?ref=2.3";
      flake = false;
    };
    korean-hunspell-src = {
      url = "github:spellcheck-ko/hunspell-dict-ko";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, jinx-src, korean-hunspell-src }:
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

        korean-hunspell = pkgs.stdenv.mkDerivation {
          name = "ko";
          src = korean-hunspell-src;
          pname = "hunspell-dict-ko";
          buildInputs = [
            pkgs.python3
            pkgs.python3.pkgs.pyaml
          ];
          installPhase = ''
            mkdir -p $out/share/hunspell
            cp ko.aff $out/share/hunspell/ko.aff
            cp ko.dic $out/share/hunspell/ko.dic
          '';
        };

        # Create the module and inject its dependency we declared above
        module = import ./jinx.nix { inherit emacs-jinx korean-hunspell; };

      in
      rec {
        homeConfigurations = {
          emacs-jinx = module;
          default = homeConfigurations.emacs-jinx;
        };
        packages = rec {
          inherit emacs-jinx korean-hunspell;
          default = packages.emacs-jinx;
        };
      });
}

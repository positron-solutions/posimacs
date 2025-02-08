{
  description = ''
    Home manager module for an Emacs installation with dotfiles integration &
    3rd party deps.
  '';

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-24.05";
    emacs-igc-src = {
      url = "github:emacs-mirror/emacs/feature/igc";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    jinx-src = {
      url = "github:minad/jinx?ref=1.12";
      flake = false;
    };
    emacs-jinx = {
      url = "path:./jinx";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.jinx-src.follows = "jinx-src";
    };
  };

    outputs = inputs:
      inputs.flake-utils.lib.eachDefaultSystem (system:
        let
          # Submodules from our inputs are injected into the home manager module
          module = import ./posimacs.nix {
            emacs-jinx = inputs.emacs-jinx.homeConfigurations.${system}.default;
            emacs-overlay = inputs.emacs-overlay;
            emacs-igc-src = inputs.emacs-igc-src;
          };
        in
        rec {
          homeConfigurations = {
            posimacs = module;
            default = homeConfigurations.posimacs;
          };
        });
}

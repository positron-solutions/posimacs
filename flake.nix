{
  description = ''
    Home manager module for an Emacs installation with dotfiles integration &
    3rd party deps.
  '';

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-24.05";
    # emacs29-src = {
    #   url = "github:emacs-mirror/emacs/emacs-29.3";
    #   flake = false;
    # };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    emacs-jinx = {
      url = "path:./jinx";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        # Submodules from our inputs are injected into the home manager module
        module = import ./posimacs.nix {
          emacs-jinx = inputs.emacs-jinx.homeConfigurations.${system}.default;
          emacs-overlay = inputs.emacs-overlay;
          # emacs29-src = inputs.emacs29-src;
        };
      in
      rec {
        homeConfigurations = {
          posimacs = module;
          default = homeConfigurations.posimacs;
        };
      });
}

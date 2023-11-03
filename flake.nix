{
  description = ''
    Home manager module for an Emacs installation with dotfiles integration &
    3rd party deps.
  '';

  inputs = {
    cargo2nix = {
      url = "github:cargo2nix/cargo2nix/?ref=release-0.11.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.rust-overlay.follows = "rust-overlay";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-23.05";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    emacs29-src = {
      url = "github:emacs-mirror/emacs/emacs-29.1";
      flake = false;
    };
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    rust = {
      url = "path:./rust";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-overlay.follows = "rust-overlay";
      inputs.flake-utils.follows = "flake-utils";
      inputs.cargo2nix.follows = "cargo2nix";
      inputs.cargo2nix.inputs.rust-overlay.follows = "rust-overlay";
    };
    emacs-vterm = {
      url = "path:./vterm";
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
          rust = inputs.rust.homeConfigurations.${system}.default;
          emacs-vterm = inputs.emacs-vterm.homeConfigurations.${system}.default;
          emacs-jinx = inputs.emacs-jinx.homeConfigurations.${system}.default;
          emacs-overlay = inputs.emacs-overlay;
	  emacs29-src = inputs.emacs29-src;
        };

      in rec {
        homeConfigurations = {
          posimacs = module;
          default = homeConfigurations.posimacs;
        };
      });
}

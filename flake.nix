{
  description = ''
    Home manager module for an Emacs installation with dotfiles integration &
    3rd party deps.
  '';

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/?rev=1f08a4df998e21f4e8be8fb6fbf61d11a1a5076a";
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
      url = "github:minad/jinx?ref=2.3";
      flake = false;
    };
    emacs-jinx = {
      url = "path:/home/satoshi/Desktop/positron/posimacs/jinx";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.jinx-src.follows = "jinx-src";
    };

    emacs-vterm-src = {
      url = "github:akermu/emacs-libvterm";
      flake = false;
    };
    emacs-vterm = {
      url = "path:/home/satoshi/Desktop/positron/posimacs/vterm";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.emacs-vterm-src.follows = "emacs-vterm-src";
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
            emacs-vterm = inputs.emacs-vterm.homeConfigurations.${system}.default;
          };
        in
          rec {
            homeConfigurations = {
              posimacs = module;
              default = homeConfigurations.posimacs;
            };
          });
}

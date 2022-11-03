{
  description = "Home manager module for Emacs rust dependencies & integration.";

  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix/?ref=release-0.11.0";
    rust-overlay.url = "github:oxalica/rust-overlay";
    nixpkgs.url = "github:nixos/nixpkgs?ref=release-22.05";
    rust-analyzer = {
      url = "path:./rust-analyzer";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.cargo2nix.follows = "cargo2nix";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, cargo2nix, nixpkgs, rust-analyzer, rust-overlay, flake-utils,}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        rust-analyzer' = rust-analyzer.packages.${system}.default;
        cargo2nix' = cargo2nix.packages.${system}.default;

        # Create the module and inject its dependency we declared above
        module = import ./rust-module.nix { inherit rust-overlay;
                                            cargo2nix = cargo2nix';
                                            rust-analyzer = rust-analyzer';};
      in rec {
        nixosModules = {
          rust = module;
          default = nixosModules.rust;
        };
      });
}

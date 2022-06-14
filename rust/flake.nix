{
  description = "Rust inputs for flake style home manager module";

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

    # These may be included due to some inadvertent `follows` behavior that needs
    # sorting out
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  # "properly defined inputs" are just passed through for consumption in modules
  outputs = inputs: inputs;
}

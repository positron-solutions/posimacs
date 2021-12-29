{
  description = "Rust inputs for flake style home manager module";

  inputs = {
    cargo2nix.url = "github:cargo2nix/cargo2nix";
    rust-overlay.url = "github:oxalica/rust-overlay";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
    rust-analyzer.url = "path:./rust-analyzer";

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

{ config, pkgs, lib, ... }:

let
  rust-analyzer = (import ./rust-analyzer.nix {}).rust-analyzer;
in {
  config = {
    home.packages = [
      rust-analyzer
    ];
  };
}

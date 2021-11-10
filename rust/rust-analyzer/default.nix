{ config, pkgs, lib, ... }:

let
  rust-analyzer = (import ./rust-analyzer.nix).default;
in {
  config = {
    home.packages = [
      rust-analyzer
    ];
  };
}

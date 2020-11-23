{ config, pkgs, lib, ... }:

let
  rust-analyzer = (import ./rust-analyzer.nix {}).package;
in {
  config = {
    home.packages = [
      rust-analyzer
    ];
  };
}

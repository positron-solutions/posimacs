{ config, pkgs, lib, specialArgs, ... }:

let
  rust-analyzer = (import ./rust-analyzer.nix).defaultPackage.${specialArgs.system};
in {
  config = {
    home.packages = [
      rust-analyzer
    ];
  };
}

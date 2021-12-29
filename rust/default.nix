{ config, pkgs, lib, specialArgs, ... }:

let
  cfg = config.posimacs-options;

  rust = specialArgs.inputs.posimacs.rust;

  rust-analyzer = rust.rust-analyzer.defaultPackage.${specialArgs.system};
  cargo2nix = rust.cargo2nix.defaultPackage.${specialArgs.system};
in let
  osSpecific = if pkgs.stdenv.isDarwin
               then [pkgs.darwin.apple_sdk.frameworks.Security]
               else [pkgs.cacert];
  rustComponents = pkgs.rust-bin."${cfg.rust-channel-type}"."${cfg.rust-version}".default;
in {
  options.posimacs-options.rust-version = lib.mkOption {
    type = lib.types.str;
    description = "Version of Rust to install, such as 1.45.0 or latest";
    example = "1.45.0";
    default = "latest";
  };

  options.posimacs-options.rust-channel-type = lib.mkOption {
    type = lib.types.str;
    description = "Nightly, stable, or beta";
    example = "nightly";
    default = "stable";
  };

  config = {
    nixpkgs.overlays = [ rust.rust-overlay.overlay ];

    home.packages = [
      cargo2nix
      pkgs.gcc
      pkgs.llvm
      rustComponents
      rust-analyzer
    ]
    ++ osSpecific;
  };
}

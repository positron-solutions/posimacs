{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs-options;

  sources = import ./nix/sources.nix;
  pinnedPkgs = import sources.nixpkgs {
    overlays = [
      (import "${sources.nixpkgs-mozilla}/rust-overlay.nix")
    ];
  };
  cargo2nix = (import sources.cargo2nix { }).package;
in let
  osSpecific = if pinnedPkgs.stdenv.isDarwin
               then [pinnedPkgs.darwin.apple_sdk.frameworks.Security]
               else [pinnedPkgs.cacert];
  rustChannel = pinnedPkgs.rustChannelOf { channel = cfg.rust-channel; };
in {
  imports = [
    ../rust-analyzer
  ];

  options.posimacs-options.rust-channel = lib.mkOption {
    type = lib.types.str;
    description = "Version of Rust to install, such as 1.45.0 or stable";
    example = "1.45.0";
    default = "stable";
  };

  config = {
    home.packages = [
      cargo2nix
      pkgs.gcc
      pinnedPkgs.llvm
      rustChannel.rust
    ]
    ++ osSpecific;
  };
}

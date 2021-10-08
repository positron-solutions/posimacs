{ config, pkgs, lib, ... }:

let
  cfg = config.posimacs-options;

  sources = import ./nix/sources.nix;
  pinnedPkgs = import sources.nixpkgs {
    overlays = [
      (import sources.rust-overlay)
    ];
  };
  cargo2nix = (import sources.cargo2nix { }).package;
in let
  osSpecific = if pinnedPkgs.stdenv.isDarwin
               then [pinnedPkgs.darwin.apple_sdk.frameworks.Security]
               else [pinnedPkgs.cacert];
  # rustChannel = pinnedPkgs.rustChannelOf {
  #   channel = cfg.rust-channel;
  # };
  rustComponents = pinnedPkgs.rust-bin."${cfg.rust-channel-type}"."${cfg.rust-version}";
in {
  imports = [
    ./rust-analyzer
  ];

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
    home.packages = [
      cargo2nix
      pkgs.gcc
      pinnedPkgs.llvm
    ] ++ (with rustComponents; [
      # Not all components are available in all toolchains
      # https://rust-lang.github.io/rustup-components-history/
      cargo
      clippy
      rust-analysis
      rust-docs
      rust-std
      rustc
      rustfmt
    ])
    ++ osSpecific;
  };
}

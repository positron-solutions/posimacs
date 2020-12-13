{
  sources ? import ./nix/sources.nix,
  system ? builtins.currentSystem,
  overlays ? [ ],
  crossSystem ? null,
}:
let
  # 1. Setup nixpkgs with nixpkgs-mozilla overlay and cargo2nix overlay.
  pkgs = import sources.nixpkgs {
    inherit system crossSystem;
    overlays =
      let
        rustOverlay = import "${sources.nixpkgs-mozilla}/rust-overlay.nix";
        cargo2nixOverlay = (import "${sources.cargo2nix}/overlay");
      in
        overlays ++ [ cargo2nixOverlay rustOverlay ];
  };

  rustPkgs = pkgs.rustBuilder.makePackageSet' {
    rustChannel = "1.48.0";
    packageFun = import ./Cargo.nix;

    workspaceSrc = pkgs.fetchFromGitHub {
      owner = "rust-analyzer";
      repo = "rust-analyzer";
      rev = "cadf0e9fb630d04367ef2611383865963d84ab54";
      sha256 = "0w5q8srjhv510398ay5m3rih3nkwcf4f2grb55f1gc2kd7m6bfww";
    };
  };

in rec {
  rust-analyzer = (rustPkgs.workspace.rust-analyzer {}).bin;
}

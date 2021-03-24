{
  sources ? import ./nix/sources.nix,
  system ? builtins.currentSystem,
  overlays ? [ ],
  crossSystem ? null,
}:
let
  pkgs = import sources.nixpkgs {
    inherit system crossSystem;
    overlays =
      let
        rustOverlay = import sources.rust-overlay;
        cargo2nixOverlay = (import "${sources.cargo2nix}/overlay");
      in
        overlays ++ [ cargo2nixOverlay rustOverlay ];
  };

  rustPkgs = pkgs.rustBuilder.makePackageSet' {
    rustChannel = "1.50.0";
    packageFun = import ./Cargo.nix;

    workspaceSrc = pkgs.fetchFromGitHub {
      owner = "rust-analyzer";
      repo = "rust-analyzer";
      rev = "5ba7852cf153688d5b5035a9a2a2145aa7334d79";
      sha256 = "150gydm0mg72bbhgjjks8qc5ldiqyzhai9z4yfh4f1s2bwdfh3yf";
    };
  };

in rec {
  rust-analyzer = (rustPkgs.workspace.rust-analyzer {}).bin;
}

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
    rustChannel = "1.56.1";
    packageFun = import ./Cargo.nix;
    localPatterns = [ ''^(src|crates|xpath|tests|templates)(/.*)?'' ''[^/]*\.(rs|toml)$'' ];
    packageOverrides = pkgs: pkgs.rustBuilder.overrides.all ++ [
      (pkgs.rustBuilder.rustLib.makeOverride {
        name = "rust-analyzer";
        overrideAttrs = drv: {
          propagatedNativeBuildInputs = drv.propagatedNativeBuildInputs or [ ] ++ [
            pkgs.gperftools
          ];
        };
      })
    ];


    workspaceSrc = pkgs.fetchFromGitHub {
      owner = "rust-analyzer";
      repo = "rust-analyzer";
      rev = "2c0f433fd2e838ae181f87019b6f1fefe33c6f54";
      sha256 = "sha256-nqRK5276uTKOfwd1HAp4iOucjka651MkOL58qel8Hug=";
    };
  };

in rec {
  rust-analyzer = (rustPkgs.workspace.rust-analyzer {}).bin;
}

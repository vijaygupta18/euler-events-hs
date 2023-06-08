{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    # haskell-flake 0.4.0 (unreleased), points to latest master commit
    haskell-flake.url = "github:srid/haskell-flake/908a59167f78035a123ab71ed77af79bed519771";
    prometheus-haskell.url = "github:juspay/prometheus-haskell/more-proc-metrics";
    prometheus-haskell.inputs.haskell-flake.follows = "haskell-flake";

  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        packages.default = self'.packages.euler-events-hs;
        haskellProjects.default = {
          imports = [
            inputs.prometheus-haskell.haskellFlakeProjectModules.output
          ];

          packages = {};

          settings = {
            prometheus-client = {
              check = false;
            };
            prometheus-proc = {
              jailbreak = true;
            };
            euler-events-hs = {
              jailbreak = true;
              check = false;
            };
          };
        };
      };
    });
}

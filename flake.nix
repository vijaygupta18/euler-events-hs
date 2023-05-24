{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake/0.3.0";
    prometheus-haskell.url = "github:juspay/prometheus-haskell/54b8fbd57257dbec0b4b76814773c5cebfae2def";
    prometheus-haskell.flake = false;
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
          source-overrides = {
            prometheus-client = inputs.prometheus-haskell + /prometheus-client;
            prometheus-proc = inputs.prometheus-haskell + /prometheus-proc;
            prometheus-metrics-ghc = inputs.prometheus-haskell + /prometheus-metrics-ghc;
            wai-middleware-prometheus = inputs.prometheus-haskell + /wai-middleware-prometheus;
          };

          overrides = self: super:
            with pkgs.haskell.lib.compose;
            lib.mapAttrs (k: v: lib.pipe super.${k} v) {
              prometheus-client = [ dontCheck ];
              prometheus-proc = [ doJailbreak ];
              euler-events-hs = [ doJailbreak dontCheck ];
            };
        };
      };
    });
}

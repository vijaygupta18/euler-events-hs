self: super:
let
  inherit (super) fetchFromGitHub eulerBuild;
 prometheus-haskell-repo = fetchFromGitHub {
    owner = "juspay";
    repo = "prometheus-haskell";
    # more-proc-metrics branch
    rev = "7bf933ad3e0059020273ab9d7fc799c582d663ae";
    sha256 = "1a00yb7258gk73idwffd2c1fvw6jci71mal143q81pgxaq9ivf78";
  };
  prometheus-client-path = "${prometheus-haskell-repo}/prometheus-client";
  prometheus-proc-path = "${prometheus-haskell-repo}/prometheus-proc";
  prometheus-metrics-ghc-path = "${prometheus-haskell-repo}/prometheus-metrics-ghc";
  wai-middleware-prometheus-path = "${prometheus-haskell-repo}/wai-middleware-prometheus";

  euler-events-hs-src = eulerBuild.allowedPaths {
    root =  ../.;
    paths = [
      ../euler-events-hs.cabal
      ../src
      ../test
    ];
  };
in
eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper: {

    prometheus-client =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-client" prometheus-client-path { };
      };

    prometheus-proc =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-proc" prometheus-proc-path { };
      };

    prometheus-metrics-ghc =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "prometheus-metrics-ghc" prometheus-metrics-ghc-path { };
      };

    wai-middleware-prometheus =
      eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "wai-middleware-prometheus" wai-middleware-prometheus-path { };
      };

    euler-events-hs =
      eulerBuild.fastBuild {
        drv = hself.callCabal2nix "euler-events-hs" euler-events-hs-src { };
        overrides = {
          runTests = true;
        };
      };

    stm-containers = eulerBuild.fastBuildExternal {
       drv = super.haskell.lib.unmarkBroken hsuper.stm-containers;
    };
    list-t = eulerBuild.fastBuildExternal {
       drv = super.haskell.lib.unmarkBroken hsuper.list-t;
    };
    stm-hamt = eulerBuild.fastBuildExternal {
       drv = super.haskell.lib.unmarkBroken hsuper.stm-hamt;
    };
    primitive-extras = eulerBuild.fastBuildExternal {
       drv = super.haskell.lib.unmarkBroken hsuper.primitive-extras;
    };
  })

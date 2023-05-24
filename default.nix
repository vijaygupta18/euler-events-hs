{
  remoteDeps ? false
, haskellCompiler ? "ghc883"
}:
let
  inherit (builtins) fromJSON readFile;
  sources = fromJSON (readFile ./nix/sources.json);

  euler-hs-repo = fetchGit sources.euler-hs;
  euler-hs-path =
    if remoteDeps
    then euler-hs-repo
    else ../euler-hs;

 euler-hs-drv = import euler-hs-path {
   inherit remoteDeps;
    # Uncomment if you want to change haskellCompiler
    # that is used by euler-hs by default:
    # inherit haskellCompiler;
  };

  inherit (euler-hs-drv) eulerBuild nixpkgs;

  euler-events-hs-src = eulerBuild.allowedPaths {
    root =  ./.;
    paths = [
      ./euler-events-hs.cabal
      ./src
      ./test
    ];
  };
  euler-events-hs-overlay = eulerBuild.importOverlay ./nix/euler-events-hs-overlay.nix {
    src = euler-events-hs-src;
  };

  allUsedOverlays = [
    euler-hs-drv.overlay
    euler-events-hs-overlay
  ];

  pkgs = import nixpkgs {
    overlays = allUsedOverlays;
  };

  haskellPackagesTools =
    with pkgs.haskellPackages;
    [
      hlint
      cabal-fmt
      nixfmt
      stylish-haskell
    ];
  tools = [];

  mkShell = eulerBuild.mkShell {
    drvPath = ./default.nix;
    drvName = "euler-events-hs";
    inherit haskellPackagesTools;
    inherit tools;
  };
in {
  inherit pkgs;
  euler-events-hs = pkgs.eulerHaskellPackages.euler-events-hs;

  inherit euler-events-hs-overlay;
  overlay = eulerBuild.composeOverlays allUsedOverlays;

  inherit mkShell;
}

# add librdkafka as a dependency for hw-kafka-client

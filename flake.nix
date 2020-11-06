{
  description = "Morpho checkpointing node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix/update-flake";
    nixpkgs.follows = "haskell-nix";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, haskell-nix, ... }: (utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
    (system: rec {
      morpho-pkgs = import ./nix/morpho-node.nix {
        pkgs = (import haskell-nix.sources.nixpkgs (haskell-nix.nixpkgsArgs // { inherit system; })).pkgs;
        src = ./.;
        profile = false;
        haskellCompiler = "ghc865";
      };
      morpho-node = morpho-pkgs.morpho-checkpoint-node.components.exes.morpho-checkpoint-node;
      defaultPackage = morpho-node;
    }));
}

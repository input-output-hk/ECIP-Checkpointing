{ system ? builtins.currentSystem }:
let
  sources = import ./sources.nix;
  haskell-nix = import sources."haskell.nix" { inherit system; };
  nixpkgs = haskell-nix.sources.nixpkgs-2003;
  pkgs = (import nixpkgs haskell-nix.nixpkgsArgs).pkgs;
in {
  inherit haskell-nix pkgs;
  nixpkgs = nixpkgs { inherit system; };
  niv = (import sources.niv {}).niv;
  nix-tools = pkgs.haskell-nix.nix-tools;
}

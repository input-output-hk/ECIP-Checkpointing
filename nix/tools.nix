let
  sources = import ./sources.nix;
  haskell-nix = import sources."haskell.nix" {};
  nixpkgs = haskell-nix.sources.nixpkgs-2003;
  pkgs = (import nixpkgs haskell-nix.nixpkgsArgs).pkgs;
in {
  inherit nixpkgs haskell-nix pkgs;
  niv = (import sources.niv {}).niv;
  nix-tools = pkgs.haskell-nix.nix-tools;
}

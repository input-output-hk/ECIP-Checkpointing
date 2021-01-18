{ haskellCompiler ? "ghc865"
, profile ? false
, src ? builtins.fetchGit ./.
, system ? builtins.currentSystem
, tools ? import ./nix/tools.nix { inherit system; }
}:
let
  sources = import ./nix/sources.nix;
  pkgs = tools.pkgs;
  lib = tools.pkgs.lib;
  mantis = import
    (pkgs.fetchFromGitHub {
      fetchSubmodules = true;
      owner = "input-output-hk";
      repo = "mantis";
      rev = "6e76301275659f7c096d4ed4ebdc4de36b3c7e4a";
      sha256 = "sha256-kFLfYQ+8ogz4uycvriAszwP3Af7yqRGrxH6l6HmnKuc=";
    })
    { inherit system; };
  morphoPkgs = nixShell: import ./nix/morpho-node.nix { inherit pkgs src haskellCompiler nixShell profile; };
  shell = (morphoPkgs true).shellFor {
    packages = ps: with ps; [
      morpho-checkpoint-node
    ];
    withHoogle = true;
    tools = {
      cabal = "3.2.0.0";
      haskell-language-server = "0.8.0";
    };
    buildInputs = with pkgs.haskellPackages;
      [
        ghcid
        hlint
        ormolu
        stylish-haskell
        tools.niv
        mantis
      ];
    exactDeps = true;
  };
  # Instantiate a package set using the generated file.
in morphoPkgs false // {
  inherit shell pkgs mantis sources;
}

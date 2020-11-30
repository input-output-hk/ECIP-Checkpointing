{ haskellCompiler ? "ghc865"
, profile ? false
, src ? builtins.fetchGit ./.
, system ? builtins.currentSystem
, tools ? import ./nix/tools.nix { inherit system; }
}:
let
  pkgs = tools.pkgs;
  lib = tools.pkgs.lib;
  mantis = import
    (pkgs.fetchFromGitHub {
      fetchSubmodules = true;
      owner = "input-output-hk";
      repo = "mantis";
      rev = "f4fa32b7589e0bf6044f95ccb6db202cbe039a6c";
      sha256 = "sha256-h/oVh+xnCzIPOUS+5onGuGmOIOIl4tjVu3wcAWPZcYA=";
    })
    { inherit system; };
  morphoPkgs = import ./nix/morpho-node.nix { inherit pkgs src haskellCompiler profile; };
  shell = morphoPkgs.shellFor {
    packages = ps: with ps; [
      morpho-checkpoint-node
    ];
    withHoogle = true;
    tools = {
      cabal = "3.2.0.0";
    };
    buildInputs = with pkgs.haskellPackages;
      [
        ghcid
        hlint
        ormolu
        pkgs.pkgconfig
        stylish-haskell
        tools.niv
        mantis
      ] ++
      # lobemo is depending on libsystemd for the journald bindings.
      # Systemd won't build on darwin, checking first we're not on a
      # Darwin env.
      (pkgs.stdenv.lib.optional (!pkgs.stdenv.isDarwin) pkgs.systemd);
    #exactDeps = false;
  };
  # Instantiate a package set using the generated file.
in
morphoPkgs // { inherit shell pkgs; }

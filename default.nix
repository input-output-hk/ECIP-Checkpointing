{
  haskellCompiler ? "ghc865",
  profile ? false,
  src ? builtins.fetchGit ./.,
  system ? builtins.currentSystem,
  tools ? import ./nix/tools.nix { inherit system; }
}:
let
  pkgs = tools.pkgs; # TODO is this correct?
  lib = tools.pkgs.lib;
  morphoPkgs = import ./nix/morpho-node.nix { inherit pkgs src haskellCompiler profile; };
  shell = morphoPkgs.shellFor {
    packages = ps: with ps; [
      morpho-checkpoint-node
    ];
    withHoogle = true;
    tools = {
      cabal = "3.2.0.0";
    };
    buildInputs = with tools.pkgs.haskellPackages;
    [
      ghcid
      hlint
      ormolu
      pkgs.pkgconfig
      stylish-haskell
      tools.niv
    ] ++
    # lobemo is depending on libsystemd for the journald bindings.
    # Systemd won't build on darwin, checking first we're not on a
    # Darwin env.
    (pkgs.stdenv.lib.optional (!pkgs.stdenv.isDarwin) pkgs.systemd);
    #exactDeps = false;
  };
  # Instantiate a package set using the generated file.
in morphoPkgs // { inherit shell pkgs; }

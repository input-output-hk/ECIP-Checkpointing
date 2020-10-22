{
  haskellCompiler ? "ghc865",
  profile ? false,
  src ? builtins.fetchGit ./.
}:
let
  tools = import ./nix/tools.nix;
  pkgs = tools.pkgs; # TODO is this correct?
  lib = tools.pkgs.lib;
  hsPkgs =
  (tools.pkgs.haskell-nix.cabalProject {
    inherit src;
    name = "morpho-checkpoint-node";
    compiler-nix-name = haskellCompiler;
    pkg-def-extras = [
      (hackage: {
        packages = {
          "quiet" = (((hackage.quiet)."0.2").revisions).default;
        };})
    ];
    modules = [
    #   # Specific package overrides would go here for example:
    #   packages.cbors.package.ghcOptions = "-Werror";
    #   packages.cbors.patches = [ ./one.patch ];
    #   packages.cbors.flags.optimize-gmp = false;
    (lib.recursiveUpdate {
      # Some pkgs fail to build haddock documentation
      packages.bytestring-builder.doHaddock = false;
      packages.fail.doHaddock = false;
      packages.mtl-compat.doHaddock = false;
      packages.dns.doHaddock = false;
      packages.matrix.doHaddock = false;
      packages.terminfo.doHaddock = false;
    }  (lib.optionalAttrs profile {
      enableLibraryProfiling = true;
      packages.morpho-checkpoint-node.enableExecutableProfiling = true;
    }))
    #   # It may be better to set flags in `cabal.project` instead
    #   # (`plan-to-nix` will include them as defaults).
    ];
    pkgs = [
      "canonical-json"
    ];
    extra-hackages = [
    ];
  });
  shell = hsPkgs.shellFor {
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
in hsPkgs // { inherit shell pkgs; }

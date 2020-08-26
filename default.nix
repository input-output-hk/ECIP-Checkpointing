{
  haskellCompiler ? "ghc865",
  profile ? false
}:
let
  tools = import ./nix/tools.nix;
  lib = tools.pkgs.lib;
  hsPkgs =
  (tools.pkgs.haskell-nix.cabalProject {
    src = tools.pkgs.haskell-nix.haskellLib.cleanGit {
      name = "blockchain-checkpoint-node";
      src = ./.;
    };
    compiler-nix-name = haskellCompiler;
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
      packages.blockchain-checkpoint-node.enableExecutableProfiling = true;
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
    name = "blockchain-checkpoint-node-shell";
    packages = ps: with ps; [
      blockchain-checkpoint-node
    ];
    withHoogle = true;
    buildInputs = with tools.pkgs.haskellPackages;
    [ hlint stylish-haskell ghcid tools.niv tools.pkgs.haskell-nix.cabal-install ];
    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = false;
  };
  # Instantiate a package set using the generated file.
in hsPkgs // { inherit shell; }

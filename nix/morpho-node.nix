{ pkgs
, src
, haskellCompiler ? "ghc865"
, profile
, nixShell
, ...
}:

pkgs.haskell-nix.cabalProject {
  inherit src;
  name = "morpho-checkpoint-node";
  compiler-nix-name = haskellCompiler;

  pkg-def-extras = [
    (hackage: {
      packages = {
        "quiet" = (((hackage.quiet)."0.2").revisions).default;
      };
    })
  ];
  modules = [
    #   # Specific package overrides would go here for example:
    #   packages.cbors.package.ghcOptions = "-Werror";
    #   packages.cbors.patches = [ ./one.patch ];
    #   packages.cbors.flags.optimize-gmp = false;
    (pkgs.lib.recursiveUpdate
      {
        # Some pkgs fail to build haddock documentation
        packages.bytestring-builder.doHaddock = false;
        packages.fail.doHaddock = false;
        packages.mtl-compat.doHaddock = false;
        packages.dns.doHaddock = false;
        packages.matrix.doHaddock = false;
        packages.terminfo.doHaddock = false;
      }
      (pkgs.lib.optionalAttrs profile {
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
}

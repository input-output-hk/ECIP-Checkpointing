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

  # For source-repository-package's, cabal tries to build them on its own, even
  # when all dependencies are already provided by Nix. Relevant issues:
  # - https://github.com/haskell/cabal/issues/6049
  # - https://github.com/input-output-hk/ouroboros-network/pull/645
  # - https://github.com/haskell/cabal/issues/5586#issuecomment-479576084
  #
  # This seems to be a problem even with a cabal that includes
  # https://github.com/haskell/cabal/pull/6917 (see
  # https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
  # for how to test a cabal-install 3.4)
  #
  # The only known workaround is to remove the source-repository-package
  # sections from cabal.project, but this should only be done for cabal when
  # used from a nix-shell, not from cabal without a nix-shell, and not outside
  # the nix-shell.
  #
  # To make this work smoothly, the script `scripts/nix-setup` can be used,
  # which splits the source-repository-package sections into cabal.project.srcs,
  # which is then again included from here (to make the Nix setup still work).
  # Running the script again undoes it.
  cabalProject =
    let
      exists = builtins.pathExists (../cabal.project.srcs);
      standard = builtins.readFile (src + "/cabal.project");
      sources = builtins.readFile (../cabal.project.srcs);
    in if exists then standard + "\n" + sources
    else if nixShell then throw "You need to run scripts/nix-setup first"
    else standard;

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

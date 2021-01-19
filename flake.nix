{
  description = "Morpho checkpointing node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix/flake-sources";
    utils.url = "github:numtide/flake-utils";
    mantis-src.url = "github:input-output-hk/mantis";
    mantis-src.flake = false;
  };

  outputs = { mantis-src, utils, haskell-nix, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import haskell-nix.sources.nixpkgs-2003 {
        config = import (haskell-nix + "/config.nix");
        overlays = [ haskell-nix.overlay ];
        localSystem.system = system;
      };

      mantis = import mantis-src {
        src = mantis-src;
        inherit system;
      };

      profile = false;

      hpkgs = pkgs.haskell-nix.cabalProject {

        name = "morpho-checkpoint-node";
        plan-sha256 = "05is7yxccpcvxx0pbfrs1rad81a5jdqn74r6zlp1z9qzxmq0gif1";
        src = ./.;
        compiler-nix-name = "ghc865";

        modules = [
          # {
          #   # Specific package overrides would go here for example:
          #   packages.cbors.package.ghcOptions = "-Werror";
          #   packages.cbors.patches = [ ./one.patch ];
          #   packages.cbors.flags.optimize-gmp = false;
          # }
          ({ lib, ... }: lib.mkIf profile {
            # It may be better to set flags in `cabal.project` instead
            # (`plan-to-nix` will include them as defaults).
            enableLibraryProfiling = true;
            packages.morpho-checkpoint-node.enableExecutableProfiling = true;
          })
        ];
      };

      shell = hpkgs.shellFor {
        packages = p: [ p.morpho-checkpoint-node ];
        exactDeps = true;
        withHoogle = true;
        tools = {
          cabal = {};
          ghcid = {};
          ormolu = {};
          hlint = {};
          haskell-language-server = "0.8.0";
        };
        nativeBuildInputs = [
          #mantis

          # Used by scripts/bin/cabal
          pkgs.bubblewrap
        ];
        shellHook = ''
          # Used by scripts/bin/cabal to avoid calling itself
          export ORIGINAL_CABAL=$(command -v cabal)
          export PATH=$PWD/scripts/bin:$PATH
        '';
      };
    in {
      inherit mantis;
      defaultPackage = hpkgs.morpho-checkpoint-node.components.exes.morpho-checkpoint-node;
      devShell = shell;
    });
}

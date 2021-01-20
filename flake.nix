{
  description = "Morpho checkpointing node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix/flake-sources";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, utils, haskell-nix, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import haskell-nix.sources.nixpkgs-2003 {
        config = import (haskell-nix + "/config.nix");
        overlays = [ haskell-nix.overlay ];
        localSystem.system = system;
      };

      inherit (pkgs) lib;

      mantisSrc = builtins.fetchGit {
        url = "https://github.com/input-output-hk/mantis";
        rev = "dac4214e78cf0ed103f2194accf9f5a1e44d6062";
        ref = "develop";
        submodules = true;
      };

      mantis = import mantisSrc { inherit system; };

      profile = false;
      src = ./.;

      hpkgs = pkgs.haskell-nix.cabalProject {

        name = "morpho-checkpoint-node";
        plan-sha256 = "05is7yxccpcvxx0pbfrs1rad81a5jdqn74r6zlp1z9qzxmq0gif1";
        src = src;
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
          mantis

          # Used by scripts/bin/cabal
          pkgs.bubblewrap
        ];
        shellHook = ''
          # Used by scripts/bin/cabal to avoid calling itself
          export ORIGINAL_CABAL=$(command -v cabal)
          export PATH=$PWD/scripts/bin:$PATH
        '';
      };
      morpho = hpkgs.morpho-checkpoint-node;

      checks = {

        unit = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.test.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
          }));

        integration = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.mantis-integration-tests.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
            nativeBuildInputs = old.nativeBuildInputs ++ [ mantis ];
          }));

        statemachine = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.state-machine-tests.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
          }));

        formatting = shell.overrideAttrs (old: {
          inherit src;
          phases = [ "unpackPhase" "installPhase" ];
          installPhase = ''
            ec=0

            # Check cabal files format.
            # NOTE: there's no check option to the CLI, we have to check
            # whether or not the formatter is doing something :/
            for f in $(find . -type f -name "*.cabal"); do
              h=$(sha256sum "$f" | cut -d ' ' -f 1)
              cabal format "$f"
              nh=$(sha256sum "$f" | cut -d ' ' -f 1)
              if [[ ! $h == $nh ]]; then
                echo "========================"
                echo " INCORRECT FORMAT FOR $f"
                echo " PLEASE FORMAT $f WITH"
                echo " cabal format $f"
                echo "========================"
                echo ""
                ec=1
              fi
            done

            # Check haskell formatting.
            for f in $(find morpho-checkpoint-node -type f -name "*.hs"); do
                if ! ormolu -m check -c "$f"; then
                    echo "========================"
                    echo " INCORRECT FORMAT FOR $f"
                    echo " PLEASE FORMAT $f WITH"
                    echo " ormolu -m inplace $f"
                    echo "========================"
                    echo ""
                    ec=1
                fi
            done
            if [[ "$ec" == 0 ]]; then
              touch $out
            else
              exit "$ec"
            fi
          '';
        });
      };

    in {
      #inherit pkgs mantis hpkgs;
      packages.morpho = morpho.components.exes.morpho-checkpoint-node;
      packages.checks = pkgs.linkFarm "morpho-checkpoint-node-checks"
        (lib.forEach (lib.attrNames checks) (n: {
          name = n;
          path = checks.${n};
        })) // checks;
      hydraJobs = {
        inherit (self.outputs) packages devShell;
      };
      defaultPackage = self.packages.${system}.morpho;
      devShell = shell;
    });
}

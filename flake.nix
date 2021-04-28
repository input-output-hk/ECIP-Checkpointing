{
  description = "Morpho checkpointing node";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    utils.url = "github:numtide/flake-utils";
    mantis.url = "github:input-output-hk/mantis/00a44422cd9a8ade2c6a93859cd369b6f1b0225b";
  };

  outputs = { self, utils, mantis, haskell-nix, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = haskell-nix.legacyPackages.${system}.extend (final: prev: prev.lib.recursiveUpdate prev {
        haskell-nix.custom-tools.haskell-language-server.latest = args: (final.haskell-nix.cabalProject (args // {
          name = "haskell-language-server";
          src = final.fetchFromGitHub {
            owner = "haskell";
            repo = "haskell-language-server";
            rev = "1.0.0";
            sha256 = "0p0rhhc6pldzan85qp3nhc54zwabah8r3dvxdzw49i32dvy4xxgs";
            fetchSubmodules = true;
          };
        })).haskell-language-server.components.exes.haskell-language-server;
      });

      inherit (pkgs) lib;

      profile = false;
      src = ./.;

      hpkgs = pkgs.haskell-nix.cabalProject {

        name = "morpho-checkpoint-node";
        src = src;
        compiler-nix-name = "ghc8104";

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
          eventlog2html = {};
          hlint = {};
          haskell-language-server = "latest";
        };
        nativeBuildInputs = [
          self.packages.${system}.mantis

          # Used by scripts/bin/cabal
          (pkgs.bubblewrap.overrideAttrs (old: {
            patches = old.patches or [] ++ [
              # https://github.com/containers/bubblewrap/pull/402
              # Patch for bubblewrap to forward SIGINT (Ctrl-C) to the running
              # process, allowing Ctrl-C in cabal repl to properly clear the
              # current line
              (pkgs.fetchpatch {
                url = "https://github.com/containers/bubblewrap/pull/402/commits/77bc87e6f9042000a56091539ce2ca66660cd068.patch";
                sha256 = "08psqg7bkapg9cgipszjs6xh6pcjcg0la6p5rp4abi7il6cyj0fj";
              })
            ];
          }))
        ];
        shellHook = ''
          source scripts/wrap-cabal/wrap-cabal.sh
        '';
      };
      morpho = hpkgs.morpho-checkpoint-node;

      checks = {

        test = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.test.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
          }));

        mantis-integration-tests = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.mantis-integration-tests.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
            nativeBuildInputs = old.nativeBuildInputs ++ [ self.packages.${system}.mantis ];
          }));

        state-machine-tests = pkgs.haskell-nix.haskellLib.check
          (morpho.components.tests.state-machine-tests.overrideAttrs (old: {
            srcSubDir = "morpho-checkpoint-node";
          }));
      };

      checkFormatting = shell.overrideAttrs (old: {
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

    in {
      packages.morpho = morpho.components.exes.morpho-checkpoint-node;
      packages.mantis = mantis.defaultPackage.${system};
      packages.checkFormatting = checkFormatting;
      packages.checks = pkgs.linkFarm "morpho-checkpoint-node-checks"
        (lib.forEach (lib.attrNames checks) (n: {
          name = n;
          path = checks.${n};
        })) // checks;
      defaultPackage = self.packages.${system}.morpho;
      devShell = shell;
    }) // {
      hydraJobs = {
        packages = self.packages.x86_64-linux;
        devShell = self.devShell.x86_64-linux;
      };
    };
}

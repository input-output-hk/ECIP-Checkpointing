{ sources                     ? import nix/sources.nix
, nixpkgs                     ? sources.nixpkgs
, jobsets                     ? null
, src                         ? builtins.fetchGit ./.
, supportedSystems            ? [ builtins.currentSystem ]
}:
let
  default = (import src { inherit src; });
  pkgs = default.pkgs;
in {
  checkpointing-node-exe = default.morpho-checkpoint-node.components.exes;
  checkpointing-node-shell = default.shell;
  checkpointing-node-run-tests = default.pkgs.stdenvNoCC.mkDerivation {
    inherit src;
    name = "checkpointing-node-run-tests";
    installPhase = ''
      cd morpho-checkpoint-node
      ${default.morpho-checkpoint-node.components.tests.test}/bin/test
      touch $out'';
  };
  check-code-formatting = pkgs.stdenvNoCC.mkDerivation {
    inherit src;
    name = "check-code-format";
    nativeBuildInputs = [
      pkgs.ormolu
      pkgs.cabal-install
    ];
    installPhase = ''
      LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive
      export LANG=C.UTF-8.
      export LC_ALL=C.UTF-8.
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
      if [[ $ec == 0 ]]; then
        touch $out
      fi'';
  };
}

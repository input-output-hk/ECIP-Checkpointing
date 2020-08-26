{ sources                     ? import nix/sources.nix
, nixpkgs                     ? sources.nixpkgs
, jobsets                     ? null
, blockchain-checkpointing-node ? builtins.fetchGit ./.
, src                         ? blockchain-checkpointing-node
, supportedSystems            ? [ builtins.currentSystem ]
}:
let
  default = (import src { });
in {
  checkpointing-node-exe = default.blockchain-checkpoint-node.components.exes.blockchain-checkpoint-node;
  checkpointing-node-shell = default.shell;
}

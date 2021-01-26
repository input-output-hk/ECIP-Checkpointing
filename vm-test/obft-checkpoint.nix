# OBFT checkpointing NixOS VM test
#
# This test is going to spin up three machines:
# - 1 "pure" midnight node in charge of mining new blocks using the
#   mocked miner.
# - 2 checkpointing nodes, running both the obft checkpoint service
#   and a regular midnight one in charge of syncing the midnight POW
#   chain.
#
# Test Scenario
# =============
#
# After making sure all the services are correctly started, we're
# going to mine a new midnight block on the miner, wait for it to
# propagate on the two checkpointing nodes and wait for the two
# checkpointing node to reach consensus and checkpoint this new block.
let
  system = builtins.currentSystem;
  nixpkgs = (import ../morpho/nix/tools.nix { inherit system; }).nixpkgs;
  pkgs = import nixpkgs { inherit system; };
  makeTest = (import "${nixpkgs}/nixos/lib/testing-python.nix" {
    inherit system pkgs;
  }).makeTest;
  mantis = (import ../morpho {}).mantis;

  nodeCount = 2;

  genKeys = name:
    let
      drv = pkgs.runCommandNoCC "generated-values-${name}.json" {
        nativeBuildInputs = [ pkgs.jq mantis ];
      } ''
        eckeygen 1 | sed -r '/^\s*$/d' > keyFile
        publicKey=$(tail -1 keyFile)
        privateKey=$(head -1 keyFile)

        jq -n >$out \
          --arg publicKey "$publicKey" \
          --arg privateKey "$privateKey" \
          '{ public : $publicKey, private: $privateKey }'
      '';
    in builtins.fromJSON (builtins.readFile drv);

  # <node name> -> {mantis,morpho} -> {public,private}
  nodeKeys = lib.listToAttrs (map (n:
    nameValuePair "node-${toString n}" (keysForNode n)
  ) (lib.range 1 nodeCount));

  mkNode = n: {

  };


in makeTest ({
  inherit system pkgs;
  inherit (pkgs) lib;
  name = "midnight-obft-checkpointing-test";

  nodes = {
    # Midnight regular node.
    # In charge of mining blocks.
    miner = { ... }: {

      imports = [
        ./mantis.nix
        ./morpho.nix
      ];

      options.nodeKeys = lib.mkOption {
        default = nodeKeys;
      };

      config = {
        virtualisation.memorySize = 4096;
        virtualisation.diskSize = 4096;

        users.users.root.password = "password";
        console.useXkbConfig = true;
        services.xserver.xkbVariant = "dvp";

        environment.systemPackages = [ mantis ];
      };


    };

    # Checkpointing nodes.
    #checkpoint1 = { ... }: {
    #  virtualisation.memorySize = 2048;
    #  virtualisation.diskSize = 4096;
    #  services.midnight.node.enable = true;
    #  services.obftCheckpointNode.enable = true;
    #};

    #checkpoint2 = { ... }: {
    #  virtualisation.memorySize = 2048;
    #  virtualisation.diskSize = 4096;
    #  services.midnight.node = true;
    #  services.obftCheckpointNode = true;
    #};
  };

  skipLint = true;

  testScript = ''
    start_all()
    miner.wait_for_unit("midnight-node.service")
    # for node in checkpoint1, checkpoint2:
    #     node.wait_for_unit("midnight-node.service")
    #     node.wait_for_unit("obft-checkpoint.service")
    # Waiting for midnight to listen to the checkpoint RPC interface
    # and to actually respond by sending the latest POW block to the
    # obft node.
    #
    # This step could be spared if we had a proper systemd target
    # triggered when the midnight node gets responsive (it takes ~20
    # seconds to startup on my machine)
    #
    #for node in checkpoint1, checkpoint2:
    #    node.wait_until_succeeds(
    #        "journalctl -u obft-checkpoint.service | grep 'responseResult = MidnightLatestBlockResult'"
    #    )
    # Mining one block then waiting for it to be checkpointed by the
    # checkpoint federation
    #
    # Note: the block numbers are starting at 0.
    #miner.succeed("mine-midnight-block")
    #for node in checkpoint1, checkpoint2:
    #    node.wait_until_succeeds(
    #        "journalctl -u obft-checkpoint.service | grep 'number = PowBlockNo 1'"
    #    )
  '';
})*/

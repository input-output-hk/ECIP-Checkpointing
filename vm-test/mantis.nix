{ config, ... }: {
  systemd.services.mantis = {
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      DynamicUser = true;
      StateDirectory = "mantis";
    };
    script = let

      jsonConfig = pkgs.writeText "mantis.json" (builtins.toJSON {
        # mantis.node-key-file should contain a value generated with eckeygen
        # mantis.consensus.coinbase set to something generated like in mantis-ops/overlay.nix:generateCoinbase
        mantis.blockchains.network = "nixos-vm";
        mantis.consensus.mining-enabled = true;
        # mantis.client-id = 

        mantis.blockchains.nixos-vm = {

          # Ethereum network identifier:
          # 1 - mainnet, 3 - ropsten, 7 - mordor
          network-id = 42;

          # Possibility to set Proof of Work target time for testing purposes.
          # null means that the standard difficulty calculation rules are used
          pow-target-time = "30 seconds";

          # Frontier block number
          frontier-block-number = "0";

          # Homestead fork block number
          # Doc: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2.md
          homestead-block-number = "0";

          # EIP-106 fork block number
          # Doc: https://github.com/ethereum/EIPs/issues/106
          eip106-block-number = "1000000000000000000";

          # EIP-150 fork block number
          # Doc: https://github.com/ethereum/EIPs/issues/150
          eip150-block-number = "0";

          # EIP-155 fork block number
          # Doc: https://github.com/ethereum/eips/issues/155
          # 3 000 000 following lead of existing clients implementation to maintain compatibility
          # https://github.com/paritytech/parity/blob/b50fb71dd1d29dfde2a6c7e1830447cf30896c31/ethcore/res/ethereum/classic.json#L15
          eip155-block-number = "0";

          # EIP-160 fork block number
          # Doc: https://github.com/ethereum/EIPs/issues/160
          eip160-block-number = "0";

          # EIP-161 fork block number (ETH Only)
          # Doc: https://github.com/ethereum/EIPs/blob/master/EIPS/eip-161.md
          eip161-block-number = "1000000000000000000";

          # EIP-170 max code size (Enabled from Atlantis fork block number)
          # Doc: https://github.com/ethereum/EIPs/issues/170
          # null value indicates there's no max code size for the contract code
          # TODO improve this configuration format as currently it is not obvious that this is enabled only from some block number
          max-code-size = "24576";

          # Difficulty bomb pause block number
          # Doc: https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1010.md
          difficulty-bomb-pause-block-number = "0";

          # Difficulty bomb continuation block number
          # Doc: https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1010.md
          difficulty-bomb-continue-block-number = "0";

          # Difficulty bomb defusion block number
          # Doc: https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1041.md
          difficulty-bomb-removal-block-number = "0";

          # Byzantium fork block number (ETH only)
          # https://github.com/ethereum/EIPs/blob/master/EIPS/eip-609.md
          byzantium-block-number = "1000000000000000000";

          # Atlantis fork block number (ETC only)
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1054
          atlantis-block-number = "0";

          # Agharta fork block number (ETC only)
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1056
          agharta-block-number = "0";

          # Phoenix fork block number (ETC only)
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1088
          phoenix-block-number = "0";

          # Constantinople fork block number (ETH only)
          # https://github.com/ethereum/pm/issues/53
          constantinople-block-number = "1000000000000000000";

          # Petersburg fork block number (ETH only)
          # https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1716.md
          petersburg-block-number = "1000000000000000000";

          # Istanbul fork block number (ETH only)
          # https://eips.ethereum.org/EIPS/eip-1679
          istanbul-block-number = "1000000000000000000";

          # Proto-treasury fork block number (ETC only, but deactivated for now)
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1098
          treasury-address = "0011223344556677889900112233445566778899";
          ecip1098-block-number = "0";

          # Checkpointing fork block number
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1097
          # Has to be equal or greater than ecip1098-block-number
          ecip1097-block-number = "0";

          # Epoch calibration block number
          # https://ecips.ethereumclassic.org/ECIPs/ecip-1099
          ecip1099-block-number = "1000000000000000000";

          # DAO fork configuration (Ethereum HF/Classic split)
          # https://blog.ethereum.org/2016/07/20/hard-fork-completed/
          dao = null;

          # Starting nonce of an empty account. Some networks (like Morden) use different values.
          account-start-nonce = "0";

          # The ID of the accepted chain
          chain-id = "0x2A";

          # Custom genesis JSON file path
          # null value indicates using default genesis definition that matches the main network
          custom-genesis-file = pkgs.writeText "mantis-genesis.json" (builtins.toJSON {
            extraData = "0x00";
            nonce = "0x0000000000000042";
            gasLimit = "0x2fefd8";
            _commentAboutDifficulty = "Set to 1000000";
            difficulty = "0xF4240";
            ommersHash = "0x1dcc4de8dec75d7aab85b567b6ccd41ad312451b948a7413f0a142fd40d49347";
            _commentAboutTimestamp = "Set to 11/05/2020 @ 12:00am (UTC)";
            timestamp = "0x5FA34080";
            coinbase = "0x0000000000000000000000000000000000000000";
            mixHash = "0x0000000000000000000000000000000000000000000000000000000000000000";
            alloc = {};
            #  ${builtins.readFile address}.balance = "1606938044258990275541962092341162602522202993782792835301376";
            #};
          });

          # Monetary policy parameters
          # Doc: https://github.com/ethereumproject/ECIPs/blob/master/ECIPs/ECIP-1017.md
          monetary-policy = {
              # Block reward in the first era
              first-era-block-reward = "5000000000000000000";

              # Reduced block reward after Byzantium fork
              first-era-reduced-block-reward = "3000000000000000000";

              # Reduced block reward after Constantinople fork
              first-era-constantinople-reduced-block-reward = "2000000000000000000";

              # Monetary policy era duration in number of blocks
              era-duration = 5000000;

              # Rate at which rewards get reduced in successive eras.
              # Value in range [0.0, 1.0]
              reward-reduction-rate = 0.2;
          };

          # if 2 competing blocktree branches are equal in terms of total difficulty and this is set to true, then gas
          # consumed in those branches will be used to resolve the tie
          # this is currently only used in ETS blockchain tests
          gas-tie-breaker = false;

          # if true, account storage will use Ethereum-specific format for storing keys/value in MPT (32 byte)
          # if false, generic storage for arbitrary length integers will be used
          eth-compatible-storage = true;

          # Set of initial nodes
          bootstrap-nodes = [
              #"enode://ff86741b7b35087b2b53f44a612b233336490d5fae10b1434619b7714fe2d5346c71427a5e126cd27b9422a4d4376c1534ef66e88c5e62d6441d2541f63de0cf@mantis-4.mantis.ws:9004",
              #"enode://f92aa66337ab1993cc7269d4295d296aefe6199b34e900eac08c514c947ec7340d46a5648ffc2da10325dbaba16bdf92aa9c0b5e51d97a7818c3f495d478ddad@mantis-1.mantis.ws:9001",
              #"enode://442e2bd50eece65f90dee0d5c6075da4e1b4bc62e36b261a52e7f393dae6a68241e4dbad868c7ecc14fed277ed72e99a289a811b6172f35fb18bdca0b7a5602c@mantis-3.mantis.ws:9003",
              #"enode://af97643f364b805d5b0e32b5356578a16afcc4fb9d1b6622998e9441eeb7795e8daf8e6b0ff3330da9879034112be56954f9269164513ece0f7394b805be3633@mantis-5.mantis.ws:9005",
              #"enode://d8a010f019db37dcaf2e1fb98d4fcbf1f57dbd7e2a7f065e92fbe77dca8b9120d6e79f1617e98fa6134e6af8858ac8f3735b1e70a5708eb14f228080356eb0a7@mantis-2.mantis.ws:9002"
          ];

          # List of hex encoded public keys of Checkpoint Authorities
          # TODO
          checkpoint-public-keys = [];

          # List of hex encoded public keys of miners which can extend chain (only used when using restricted-ethash consensus)
          # empty means that everybody can mine
          allowed-miners = [];
        };
      });

      configFile = pkgs.writeText "mantis.conf" ''
        include "${mantis}/conf/base-testnet.conf"
        include "${jsonConfig}"
      '';
    in ''
      ${mantis}/bin/mantis \
        -Duser.home=$STATE_DIRECTORY \
        -Dconfig.file=${configFile}
    '';
  };
}

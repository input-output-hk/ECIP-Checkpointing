# Configuration reference

Documents all supported fields in the Morpho [YAML](https://yaml.org/) configuration file. The configuration file name is passed with the `--config` CLI option. Some fields can also be overridden with a CLI option.

### `NumCoreNodes`

**Type:** integer

**Required:** yes

The number of nodes in the OBFT federation. This value along with [`NodeId`](#nodeid) determines which slot numbers this node can forge blocks for.

### `NodeId`

**Type:** 0-indexed positive integer

**Required:** yes

The id of the node within the OBFT federation. This value along with [`NumCoreNodes`](#numcorenodes) determines which slot numbers this node can forge blocks for. Note that nodes are 0-indexed.

### `Protocol`

**Type:** string `MockedBFT`

**Required:** yes

Which version of the underlying OBFT protocol to use. :warning: Currently, only an insecure mocked version for easy debugging is available, providing no cryptographic security. See the [crypto document](../explanations/crypto.md#obft-related) for more information.

### `NetworkMagic`

**Type:** 32-bit unsigned integer

**Required:** yes

Arbitrary number that _must_ match among members of the OBFT federation in the same network. A random value for this can be generated with the command `shuf -i 0-4294967295 -n 1`.

**Example:** 1616802227

### `SystemStart`

**Type:** ISO 8601 date string

**Required:** yes

Start time of the network. This needs to be in the past, and the same across all federation nodes. This determines the time of the first OBFT slot. A valid value for the current time can be generated with the command `date -Iseconds`.

**Example:** `2021-03-27T22:25:45+01:00`

### `SlotDuration`

**Type:** integer (seconds)

**Required:** yes

The number of seconds per OBFT slot.

**Example:** `5`

### `SecurityParam`

**Type:** integer

**Required:** yes

The number of blocks that can be rolled back. If chains differ by more than this amount, an OBFT hard fork occurs. A value of about 2000 is the current recommendation.

**Example:** 2000

### `FedPubKeys`

**Type:** list of strings

**Required:** yes

Hex-encoded 256-bit secp256k1 ECDSA public keys of all the nodes in the OBFT federation (including the own node). See [`NodePrivKeyFile`](#nodeprivkeyfile) for info on how to generate a key pair for a node. See the [crypto document](../explanations/crypto.md) for how this is used.

**Example:** `9aa5b5a7ba70b6f29c1d721f3cda3c3e5eb6d6836a608b0bc0866a780475e5f24793a07d8ff93dd49cb0e3938d8cd0fae7e69cfffedfe638ae5090b46307c1a8`

### `CheckpointInterval`

**Type:** positive integer

**Required:** yes

The interval at which to generate checkpoints in the PoW chain. A value of 4 means that every 4th block on the proof-of-work chain is a checkpoint, starting with block number 4.

**Example:** `4`

### `RequiredMajority`

**Type:** positive integer

**Required:** yes

The number of votes for the same proof-of-work block required to generate a checkpoint. Should be more than half of [`NumCoreNodes`](#numcorenodes) but not more than all nodes.

### `StableLedgerDepth`

**Type:** integer

**Required:** yes

The number of OBFT blocks needed in order for a past block to be considered stable. With `t` being the number of byzantine nodes, this value should be `3t + 1`. See the [OBFT paper](https://iohk.io/en/research/library/papers/ouroboros-bfta-simple-byzantine-fault-tolerant-consensus-protocol/) for the reasoning behind this.

**Example:** `4`

### `PoWNodeRpcUrl`

**Type:** string (url)

**Required:** yes

The JSON-RPC HTTP endpoint of the proof-of-work node that should be queried for [RPC calls](../references/rpc.md).

**Example:** `http://127.0.0.1:8546`

### `PoWBlockFetchInterval`

**Type:** integer (microseconds)

**Required:** no, defaults to `1000000` (1 second)

The interval in microseconds at which to poll the proof-of-work node for checkpointing candidates.

**Example:** `5000000` (5 seconds)

### `NodeHost` / `--host-addr HOST-NAME`

**Type:** host string

**Required:** no, defaults to localhost

Host where the node listens for connections of other nodes.

### `NodePort` / `--port PORT`

**Type:** integer (port)

**Required:** yes

Port where the node listens for connections of other nodes.

### `DatabaseDirectory` / `--database-path FILEPATH`

**Type:** directory path

**Required:** yes

Directory to store the node's data.

### `SnapshotsOnDisk`

**Type:** positive integer

**Required:** no, defaults to 60

How many snapshots of the Morpho ledger state to keep on disk.

### `SnapshotInterval`

**Type:** positive integer

**Required:** no, defaults to 60

At which interval to take snapshots of the ledger state.

### `ValidateDatabase` / `--validate-db`

**Type:** boolean

**Required:** no, defaults to `False`

Whether to validate the full database when starting.

### `NodePrivKeyFile`

**Type:** file path

**Required:** yes

Path to the file containing the hex-encoded 128-bit secp256k1 ECDSA private key for this node, used to sign votes. The corresponding public key needs to be present in the other nodes [`FedPubKeys`](#fedpubkeys) in order for them to accept votes from this node. See the [crypto document](../explanations/crypto.md) for how this is used.

Key pairs can be generated with the [Mantis](https://github.com/input-output-hk/mantis) CLI:
```bash
$ mantis cli generate-key-pairs
363f16e6b16d72e2b02dab0d7d725fb90af6bffa4d54f53d20d4c64c6fd69c2c
aa7bed4e9fde183a5297528eec35551d4b09b554c8f867fbd035508e339934b7860a40d957d949ca068bbbd124ee6537c22e325687593ad861294e37b8f6926f
```

The first line is the private key for use in `NodePrivKeyFile`, while the second line is for `FedPubKeys`.

### `TopologyFile` / `--topology FILEPATH`

**Type:** file path

**Required:** yes

Path to the file containing the node topology in JSON format. See [the topology file reference](./topology.md) for what this file should contain.

### `PrometheusPort`

**Type:** 16-bit positive integer (port)

**Required:** yes

The port used to publish Prometheus metrics. See [`Metrics.hs`](../../morpho-checkpoint-node/src/Morpho/Tracing/Metrics.hs) for the published gauge names.

**Example:** `12789`

### `TurnOnLogging`

**Type:** boolean

**Required:** no, defaults to `True`

Whether logging should be enabled.

### `Verbosity`

**Type:** integer

**Required:** no, defaults to 0

Logged JSON values (with e.g. an `ScJson` scribe) are recursed into up to a default recursion depth, then cut off with the string `"..."`. While usually this shouldn't be necessary, the recursion level can be increased/decreased with this option.

### `Logging`

**Type:** yaml/json value

**Required:** yes

Nested iohk-monitoring configuration. See [this example](https://github.com/input-output-hk/iohk-monitoring-framework/blob/master/iohk-monitoring/test/config.yaml) for a list of available options.

**Example:**
```yaml
Logging:

  # Only display messages that have a severity of Info or higher
  # To debug specific components, prefer using mapSeverity instead, see below
  minSeverity: Info

  # Which logging backend to use
  setupBackends:
    - KatipBK
  defaultBackends:
    - KatipBK

  # For the Katip logging backend we must set up outputs (called scribes)
  setupScribes:
    # Log json to a file
    - scKind: FileSK
      scName: node-0/logs
      scFormat: ScJson

    # Log text to stdout
    - scKind: StdoutSK
      scName: stdout
      scFormat: ScText

  defaultScribes:
    - - FileSK
      - node-0/logs

    - - StdoutSK
      - stdout

  options:
    # Uncomment to enable debug messages for a specific component
    mapSeverity:
      # morpho.morpho-init: Debug
      # morpho.chain-db: Debug
      # morpho.consensus: Debug
      # morpho.ip-subs: Debug
      # morpho.dns-subs: Debug
      # morpho.dns-resolve: Debug
      # morpho.mux: Debug
      # morpho.local-mux: Debug
      # morpho.error-policy: Debug
      # morpho.extract-state: Debug
      # morpho.rpc: Debug
      # morpho.time-travel: Debug
      # morpho.chain-sync-protocol: Debug
      # morpho.chain-sync-protocol-serialized: Debug
      # morpho.block-fetch-protocol: Debug
      # morpho.block-fetch-protocol-serialized: Debug
      # morpho.tx-submission-protocol: Debug
      # morpho.tx-submission-protocol-2: Debug
      # morpho.handshake: Debug
      # morpho.local-handshake: Debug
      # morpho.local-error-policy: Debug
      # morpho.accept-policy: Debug
      # morpho.diffusion-init: Debug
      # morpho.ledger-peers: Debug
      # morpho.consensus.chain-sync-client: Debug
      # morpho.consensus.chain-sync-server-header: Debug
      # morpho.consensus.chain-sync-server-block: Debug
      # morpho.consensus.block-fetch-decision: Debug
      # morpho.consensus.block-fetch-client: Debug
      # morpho.consensus.block-fetch-server: Debug
      # morpho.consensus.tx-in: Debug
      # morpho.consensus.tx-out: Debug
      # morpho.consensus.local-tx-submission: Debug
      # morpho.consensus.mempool: Debug
      # morpho.consensus.forge: Debug
      # morpho.consensus.blockchain-time: Debug
```


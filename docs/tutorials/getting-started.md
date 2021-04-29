# Getting started with Morpho

This tutorial describes how to get started with Morpho, from zero to having a Morpho federation running that sends checkpoints to a Mantis ETC network. You might like to read the [Morpho introduction](../explanations/introduction.md) beforehand for information on what Morpho does and how it interacts with other components.

## Installing Morpho and Mantis

At the moment, Morpho is only fully supported and tested on Linux. Accordingly, this tutorial assumes that you have access to a Linux machine.

### Installing Nix Flakes

Morpho is packaged with [Nix](https://nixos.org/). So, if you haven't already, you will need to install Nix with [these instructions](https://nixos.org/download.html#nix-quick-install). Note that this requires root access.

Next, you will need a newer Nix version with Flake support. Follow [these instructions](https://nixos.wiki/wiki/Flakes#Installing_flakes) to get there. After this step, you should have a Nix version like `2.4pre`:
```
$ nix --version
nix (Nix) 2.4pre20210326_dd77f71
```

### Configuring Binary Cache (optional)

It is recommended that you configure the binary cache. Doing so allows you to reuse precompiled artifacts. Both Morpho and Mantis have cached artifacts available with the `https://hydra.mantis.ist` binary cache, whose public key is
```
hydra.mantis.ist-1:4LTe7Q+5pm8+HawKxvmn2Hx0E3NbkYjtf1oWv+eAmTo=
```

These sections show you how the cache can be configured depending on your Nix installation.

#### On NixOS

Add this to your `/etc/nixos/configuration.nix` and run a `sudo nixos-rebuild switch`:

```nix
{
  nix = {
    binaryCaches = [
      "https://hydra.mantis.ist"
    ];
    binaryCachePublicKeys = [
      "hydra.mantis.ist-1:4LTe7Q+5pm8+HawKxvmn2Hx0E3NbkYjtf1oWv+eAmTo="
    ];
  };
}
```

#### On non-NixOS

Extend your `/etc/nix/nix.conf` to contain these entry values:

```
substituters = https://hydra.mantis.ist https://cache.nixos.org/
trusted-substituters = https://hydra.mantis.ist https://cache.nixos.org/
trusted-public-keys = hydra.mantis.ist-1:4LTe7Q+5pm8+HawKxvmn2Hx0E3NbkYjtf1oWv+eAmTo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

### Preparing for installation

At the moment, in order to imperatively install a Nix Flake-built package, the `ca-references` experimental Nix feature is required. In above Flake installation steps you will already have enabled support for the `flakes` and `nix-command` experimental features, where we can add an additional one.

#### On NixOS

Extend the `nix.extraOptions` option value to
```
{
  nix = {
    extraOptions = ''
      experimental-features = ca-references nix-command flakes
    '';
  };
}
```

Then run `sudo nixos-rebuild switch`

#### On non-NixOS

Extend the `experimental-features` value in `/etc/nix.conf` to

```
experimental-features = ca-references nix-command flakes
```

### Installing Morpho

To install the latest Morpho version, run the command
```
$ nix profile install github:input-output-hk/ECIP-Checkpointing
```

Then verify that the installation worked with
```
$ morpho-checkpoint-node --help
```

### Installing Mantis

Morpho was developed in conjunction with [Mantis](https://mantisclient.io/), that implements the server side of the [RPC interface](../references/rpc.md) required by Morpho. We will use git revision [`00a44422`](https://github.com/input-output-hk/mantis/commit/00a44422cd9a8ade2c6a93859cd369b6f1b0225b). To install this version, run the command

```
$ nix profile install github:input-output-hk/ECIP-Checkpointing#mantis
```

Then verify that the installation worked with
```
$ mantis cli --help
```

## Running a single-node Morpho federation (without Mantis)

Morpho is intended to be run by many nodes, but in this tutorial we will just run a single node. For information on how to add more nodes, see [here](../howtos/expanding-federation.md). Nodes are 0-indexed, so our only node will have id 0. Let's create a new directory for our node, which we will use in the following sections:
```
$ mkdir -p ~/morpho/node-0
$ cd ~/morpho/node-0
```

### Generating a key pair

Any Morpho node needs a private/public key pair for [signing votes](../explanations/crypto.md#vote-signing). Currently, Morpho doesn't have a way to generate such a key pair, but we can use Mantis instead, which uses the same signatures scheme.

To generate a key pair with Mantis, run the following command:
```
$ mantis cli generate-key-pairs
9309f7c91678c0bcc5f0020d86735ef3d0a5dc42d9f6fd5bff0f3f3b28bf591c
cbec9424efb9f57f94ed038d721beff76c1fa4d0e19f49aac8807ecd3f4d222894001be2e48df55a1f0f6fc5703fb2187ac9a7ee8d6c92739a2de28868a6c1ab
```

This gives us a new private key on the first line, along with its public key on the second line. We will write they keys to files for later:

```
$ echo 9309f7c91678c0bcc5f0020d86735ef3d0a5dc42d9f6fd5bff0f3f3b28bf591c \
  > private-key
$ echo cbec9424efb9f57f94ed038d721beff76c1fa4d0e19f49aac8807ecd3f4d222894001be2e48df55a1f0f6fc5703fb2187ac9a7ee8d6c92739a2de28868a6c1ab \
  > public-key
```

### Creating a topology file

Morpho has to know how to connect to other Morpho nodes, which is done with a [topology file](../references/topology.md). Since we only use a single node for the moment, our topology file will be very simple. Note that this file can be shared between all Morpho nodes. Let's create a `~/morpho/topology.json` file for our single-node topology with the following contents:
```json
[
  {
    "nodeId": 0,
    "producers": []
  }
]
```

### Creating the config file

In this section we will create the Morpho configuration file. All configuration fields are explained in [the configuration page](../references/configuration.md). The first thing we need to do to start a new Morpho network is to decide on a set of parameters that can't be changed later. The most important ones are:

**`NetworkMagic`:** To distinguish networks from each other, a unique network magic number is used. You can generate a random such number with
```
$ shuf -i 0-4294967295 -n 1
2955013855
```

**`System start`:** The time at which the Morpho network starts. The time needs to be in the past and is used to synchronize OBFT slots. A valid value for the current time can be generated with
```
$ date -Iseconds
2021-04-27T16:02:03+00:00
```

**`Slot duration`:** How many seconds each OBFT slot takes. The longer this value, the longer it takes for a checkpoint to be issued. But if the time is too fast, there can be synchronization problems. We will use `5` seconds for this example.

**`Protocol`:** The OBFT hash and signature scheme to use. Currently, only an _insecure_ mocked version is available, which we can specify with `MockedBFT`.

With this in mind, we can create our configuration file in `~/morpho/node-0/config.yaml`:

```yaml
# Our federation only has a single node, and this is the config file for that node
NumCoreNodes: 1
NodeId: 0

# The network parameters we decided on
NetworkMagic: 2955013855
SystemStart: "2021-04-27T16:02:03+00:00"
SlotDuration: 5
Protocol: MockedBFT

# How many blocks can be rolled back on the OBFT chain
SecurityParam: 2000

# The public keys of all Morpho federation members, including this node's. Copied from ~/morpho/node-0/public-key
FedPubKeys:
  - cbec9424efb9f57f94ed038d721beff76c1fa4d0e19f49aac8807ecd3f4d222894001be2e48df55a1f0f6fc5703fb2187ac9a7ee8d6c92739a2de28868a6c1ab

# Some parameters required for checkpointing
CheckpointInterval: 4
RequiredMajority: 1
StableLedgerDepth: 4

# Where to reach the proof-of-work node for RPC queries (we will configure this later), and how often to poll it (in microseconds)
PoWNodeRpcUrl: http://127.0.0.1:8546
PoWBlockFetchInterval: 5000000

# On which port to listen in order to connect to other Morpho nodes
NodePort: 3000

# Where to store data, where to find the private key and the topology file
# This is relative to the directory in which we run morpho, assumed to be ~/morpho/node-0 here
DatabaseDirectory: db
NodePrivKeyFile: private-key
TopologyFile: ../topology.json

# On which port to publish prometheus metrics
PrometheusPort: 13788

# iohk-monitoring logging configuration
Logging:

  # Only display messages that have a severity of Info or higher
  minSeverity: Info

  # Which logging backend to use
  setupBackends:
    - KatipBK
  defaultBackends:
    - KatipBK

  # For the Katip logging backend we must set up outputs (called scribes)
  setupScribes:
    - scKind: StdoutSK
      scName: stdout
      scFormat: ScText
  defaultScribes:
    - - StdoutSK
      - stdout

  options: {}
```

### Running Morpho without Mantis

Having created a configuration file, we can now start Morpho:

```
$ morpho-checkpoint-node --config ~/morpho/node-0/config.yaml
[nixos:morpho.morpho-init:Info:4] [2021-04-27 16:02:29.39 UTC] I am node id CoreNodeId 0. My producers are []
[nixos:morpho.chain-db:Info:4] [2021-04-27 16:02:29.40 UTC] Replaying ledger from genesis
[nixos:morpho.consensus.forge:Info:27] [2021-04-27 16:02:29.40 UTC] Leading slot 5
[nixos:morpho.consensus.forge:Info:27] [2021-04-27 16:02:29.40 UTC] Forged block for slot 5: MorphoBlock(hash: 0xd9410648, prevHash: Genesis, slot: 5, blockNo: 0, body: MorphoBody())
[nixos:morpho.chain-db:Info:17] [2021-04-27 16:02:29.40 UTC] Chain extended, new slot: 5
[nixos:morpho.consensus.forge:Info:27] [2021-04-27 16:02:29.40 UTC] Adopted forged block for slot 5, hash: 0xd9410648, txIds: []
...
```

However, we will soon notice that there's an error:

```
[nixos:morpho.rpc:Info:40] [2021-04-27 16:02:34.40 UTC] Starting RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x))
[nixos:morpho.rpc:Error:40] [2021-04-27 16:02:34.40 UTC] Event for RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x)) returned: JsonRpcEventError (JsonRpcHttpError (HttpExceptionRequest Request {
  host                 = "127.0.0.1"
  port                 = 8546
  secure               = False
  requestHeaders       = [("content-type","application/json")]
  path                 = "/"
  queryString          = ""
  method               = "POST"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
 (ConnectionFailure Network.Socket.connect: <socket: 20>: does not exist (Connection refused))))
```

And of course, we haven't configured the Mantis proof-of-work node to answer the RPC queries by Morpho, given with `PoWNodeRpcUrl` in the config file. We will do this in the next section.

## Mining blocks with Mantis and letting them be checkpointed by Morpho

Keep the terminal from the previous section running in the background, because in this section we will also start a Mantis node that interacts with Morpho.

### Configuring Mantis

In a new terminal we will first copy the configuration directory of mantis so we can refer to its defaults:

```
$ cd morpho/node-0
$ cp -r --no-preserve=mode "$(realpath ~/.nix-profile/conf)" mantis-conf
```

Just to see it working we will declare a local testnet not connected to any other nodes. Create a `~/morpho/node-0/mantis-conf/testnet-morpho.conf` file with following contents:

```hocon
include "base-testnet.conf"

mantis {
  blockchains {
    network = "testnet-morpho"
    testnet-morpho {
      include "chains/testnet-internal-nomad-chain.conf"
      # Ideally choose your own network id here
      network-id = 1586
      # We don't want to connect to any nodes
      bootstrap-nodes = []
      allowed-miners = []

      ecip1097-block-number = "0"
      checkpoint-public-keys = [
        # The Morpho public keys
        "cbec9424efb9f57f94ed038d721beff76c1fa4d0e19f49aac8807ecd3f4d222894001be2e48df55a1f0f6fc5703fb2187ac9a7ee8d6c92739a2de28868a6c1ab"
      ]
    }
  }

  network {
    protocol-version = 64

    rpc {
      http {
        enabled = true
        mode = "http"
        port = 8546
      }

      apis = "checkpointing"
    }
  }
  consensus {
    mining-enabled = true
  }
}
```

Notable fields are:

- `mantis.network.protocol-version = 64`: In order for Mantis nodes to be able to send checkpoints between each other
- `mantis.network.rpc.http.enabled = true`: For allowing RPC calls
- `mantis.network.rpc.http.mode = "http"`: No HTTPS for RPC, matching `PoWNodeRpcUrl`
- `mantis.network.rpc.http.port = 8546`: Use port 8546 for the RPC interface, matching `PoWNodeRpcUrl`
- `mantis.network.rpc.apis = "checkpointing"`: To enable the checkpointing RPC calls
- `mantis.blockchains.<network>.ecip1097-block-number = "0"`: Decides at which block checkpointing starts
- `mantis.blockchains.<network>.checkpoint-public-keys = [ "<public key>" ... ]`: Include all the checkpointing federation's public keys here.

See [how to configure Mantis](https://docs.mantisclient.io/how-tos/how-to-configure-mantis) for more information.

### Running Mantis and observing checkpoints

Now we can start Mantis with our config file. At first it will need to do some initialization, which can take a couple minutes:
```
$ mantis -Dconfig.file=mantis-conf/testnet-morpho.conf
2021-04-27 16:33:13,961 [akka.event.slf4j.Slf4jLogger] - Slf4jLogger started
2021-04-27 16:33:15,371 [io.iohk.ethereum.Mantis$] - Mantis app mantis/v3.2.1-SNAPSHOT/linux-amd64/oraclecorporation-openjdk64bitservervm-java-1.8.0_272
2021-04-27 16:33:15,372 [io.iohk.ethereum.Mantis$] - Using network testnet-morpho
...
2021-04-27 16:33:23,169 [i.i.e.consensus.pow.EthashDAGManager] - Generating DAG 0%
2021-04-27 16:33:25,943 [i.i.e.consensus.pow.EthashDAGManager] - Generating DAG 1%
```

After the DAG has been fully generated, it will start mining for blocks:
```
2021-04-27 16:36:45,898 [i.i.e.consensus.pow.EthashMiner] - Mining unsuccessful
2021-04-27 16:36:47,242 [i.i.e.consensus.pow.EthashMiner] - Mining successful with 395dfd78860668305bad2915b014064ade946f4ba0c14b65b959d35c70cd70d9 and nonce b6d138fe028154c8
2021-04-27 16:36:47,243 [i.i.e.b.sync.regular.RegularSync] - Block mined [number = 1, hash = 1c09c6d291983ea0da10d684473a1673b0ed877434db0abcc24abbe4ba2019b3]
```

And once the 4th block is mined, the Morpho node will issue a checkpoint for it within about 30 seconds:
```
2021-04-27 16:38:07,391 [i.i.e.b.sync.regular.RegularSync] - Block mined [number = 4, hash = 4299babaf0cd57a1937755a4a562f1cc3eaa857905f1083d4972aa2ccdbf2ece]
...
2021-04-27 16:38:33,043 [i.i.e.b.sync.regular.RegularSync] - Received new checkpoint for block 4299babaf0cd57a1937755a4a562f1cc3eaa857905f1083d4972aa2ccdbf2ece
```

This is accompanied with some identifying logs on the Morpho side:

```
[nixos:morpho.rpc:Info:40] [2021-04-27 16:38:09.95 UTC] Starting RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x))
[nixos:morpho.rpc:Info:40] [2021-04-27 16:38:09.95 UTC] Successful RPC call GetLatestBlock(interval: 4, parent: PowBlock(number: 0, hash: 0x)) returned: Just(PowBlock(number: 4, hash: 0x4299baba))
[nixos:morpho.consensus.mempool:Info:40] [2021-04-27 16:38:09.95 UTC] Transaction Transaction(id: 0xb9b7ddf6, tx: Vote(block: PowBlock(number: 4, hash: 0x4299baba), byPubkey: 0xcbec9424)) added to mempool. Number of transactions in mempool goes from 0 to 1. Bytes of mempool goes from 0 to 2000
[nixos:morpho.consensus.forge:Info:27] [2021-04-27 16:38:13.00 UTC] Forged block for slot 434: MorphoBlock(hash: 0xeb7d8ae1, prevHash: 0x7aec240c, slot: 434, blockNo: 136, body: MorphoBody(Transaction(id: 0xb9b7ddf6, tx: Vote(block: PowBlock(number: 4, hash: 0x4299baba), byPubkey: 0xcbec9424))))
...
[nixos:morpho.rpc:Info:38] [2021-04-27 16:38:33.00 UTC] Starting RPC call PushCheckpoint( Checkpoint(block: PowBlock(number: 4, hash: 0x4299baba), signedByPubkeys: [0xcbec9424]))
[nixos:morpho.rpc:Info:38] [2021-04-27 16:38:33.08 UTC] Successful RPC call PushCheckpoint( Checkpoint(block: PowBlock(number: 4, hash: 0x4299baba), signedByPubkeys: [0xcbec9424])) returned: True
```

